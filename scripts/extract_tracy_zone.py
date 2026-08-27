#!/usr/bin/env python
"""Extract a nested zone tree (plus in-window messages) from a Tracy capture.

The ``.tracy`` file is a binary, zstd-compressed event stream, so this script
does not parse it directly. Instead it shells out to the already-built
``tracy-csvexport`` tool (the same one tt-metal uses in ``tools/tracy/__init__.py``)
and reconstructs the zone hierarchy itself, because csvexport emits flat
per-instance rows with no nesting links.

csvexport columns used (see tt_metal/third_party/tracy/csvexport/src/csvexport.cpp):
  -u (unwrap):  name, src_file, src_line, zone_name, zone_text,
                ns_since_start, exec_time_ns, thread, special_parent_text
  -m (messages): MessageName, total_ns   (timestamp on the same timeline as
                                           zone start times)

Reconstruction:
  * Zones are grouped by ``thread`` and the nested-zone forest is rebuilt by
    interval containment: zone B is nested inside zone A iff they share a thread
    and ``A.start <= B.start`` and ``B.end <= A.end``. The list holding a zone's
    directly-nested zones is named ``nested``.
  * Target zones default to ``shard_or_reshard_tensor_if_required`` whose source
    file contains ``conv2d_utils.cpp`` (both overridable).

Limitations:
  * Message-to-zone mapping is by time containment because csvexport drops the
    message thread id; a message is attached to the deepest enclosing zone whose
    [start, end] contains its timestamp.
  * ``thread`` is Tracy's compressed thread id (stable within one export), not
    the OS tid.
  * ``duration_ns`` = end - start (inclusive of nested-zone time).
"""

from __future__ import annotations

import argparse
import bisect
import csv
import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path


DEFAULT_ZONE_NAME = "shard_or_reshard_tensor_if_required"
DEFAULT_SRC_FILE_CONTAINS = "conv2d_utils.cpp"
CSVEXPORT_TOOL = "tracy-csvexport"

# csv has no real upper bound on field sizes; some Tracy zone_text / message
# payloads are large, so lift the default limit.
csv.field_size_limit(sys.maxsize)


def resolve_csvexport(explicit: str | None) -> str:
    """Locate the tracy-csvexport binary.

    Search order: explicit path -> $TT_METAL_HOME/build/tools/profiler/bin ->
    build_RelWithDebInfo fallback -> PATH.
    """
    if explicit:
        candidate = Path(explicit)
        if candidate.is_file():
            return str(candidate)
        raise FileNotFoundError(f"--csvexport path does not exist: {explicit}")

    search_roots: list[Path] = []
    tt_metal_home = os.environ.get("TT_METAL_HOME")
    if tt_metal_home:
        search_roots.append(Path(tt_metal_home))
    # Walk up from the cwd and the script directory so the tool is found even
    # when this script lives outside the tt-metal checkout and TT_METAL_HOME is
    # unset.
    for anchor in (Path.cwd(), Path(__file__).resolve().parent):
        search_roots.append(anchor)
        search_roots.extend(anchor.parents)

    relative_bins = [
        Path("build") / "tools" / "profiler" / "bin" / CSVEXPORT_TOOL,
        Path("build_RelWithDebInfo") / "tools" / "profiler" / "bin" / CSVEXPORT_TOOL,
        Path("build") / "bin" / CSVEXPORT_TOOL,
        Path("build_RelWithDebInfo") / "bin" / CSVEXPORT_TOOL,
    ]

    seen = set()
    for root in search_roots:
        if root in seen:
            continue
        seen.add(root)
        for rel in relative_bins:
            candidate = root / rel
            if candidate.is_file():
                return str(candidate)

    # Fall back to PATH.
    from shutil import which

    on_path = which(CSVEXPORT_TOOL)
    if on_path:
        return on_path

    raise FileNotFoundError(
        f"Could not find {CSVEXPORT_TOOL}. Pass --csvexport, set TT_METAL_HOME, "
        "or build the profiler tools (build/tools/profiler/bin)."
    )


def run_csvexport(csvexport: str, tracy_file: str, extra_args: list[str], out_path: Path) -> None:
    """Run tracy-csvexport with extra_args, writing stdout to out_path."""
    cmd = [csvexport, *extra_args, tracy_file]
    with open(out_path, "w") as out_file:
        subprocess.run(cmd, check=True, stdout=out_file)


def parse_zones(unwrap_csv: Path) -> list[dict]:
    """Parse the -u CSV into a list of zone-instance dicts."""
    zones = []
    with open(unwrap_csv, newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            start_raw = row.get("ns_since_start")
            dur_raw = row.get("exec_time_ns")
            if not start_raw or not dur_raw:
                continue
            try:
                start = int(start_raw)
                duration = int(dur_raw)
            except ValueError:
                continue
            try:
                src_line = int(row.get("src_line") or 0)
            except ValueError:
                src_line = 0
            zones.append(
                {
                    "name": row.get("name", ""),
                    "src_file": row.get("src_file", ""),
                    "src_line": src_line,
                    "zone_name": row.get("zone_name", ""),
                    "zone_text": row.get("zone_text", ""),
                    "thread": row.get("thread", ""),
                    "start_ns": start,
                    "end_ns": start + duration,
                    "duration_ns": duration,
                    "messages": [],
                    "nested": [],
                }
            )
    return zones


def parse_messages(messages_csv: Path) -> list[dict]:
    """Parse the -m CSV (semicolon-separated) into a list of message dicts."""
    messages = []
    with open(messages_csv, newline="") as f:
        reader = csv.DictReader(f, delimiter=";")
        for row in reader:
            time_raw = row.get("total_ns")
            if not time_raw:
                continue
            try:
                time_ns = int(time_raw)
            except ValueError:
                continue
            messages.append({"time_ns": time_ns, "text": row.get("MessageName", "")})
    return messages


def build_forest(zones: list[dict]) -> list[dict]:
    """Reconstruct nested-zone forests per thread via interval containment.

    Each zone's directly-nested zones are placed in its ``nested`` list. Returns
    the list of top-level (non-enclosed) zones across all threads.
    """
    by_thread: dict[str, list[dict]] = {}
    for zone in zones:
        by_thread.setdefault(zone["thread"], []).append(zone)

    roots = []
    for thread_zones in by_thread.values():
        # Sort so an enclosing zone always precedes the zones it contains:
        # earlier start first; on a tie, the longer (later-ending) zone first.
        thread_zones.sort(key=lambda z: (z["start_ns"], -z["end_ns"]))
        stack: list[dict] = []
        for zone in thread_zones:
            # Pop any zone on the stack that does not enclose the current one.
            while stack and stack[-1]["end_ns"] < zone["end_ns"]:
                stack.pop()
            # Guard against zero-overlap edge cases where the top starts after
            # the current zone (cannot happen given the sort, but stay safe).
            while stack and stack[-1]["start_ns"] > zone["start_ns"]:
                stack.pop()
            if stack:
                stack[-1]["nested"].append(zone)
            else:
                roots.append(zone)
            stack.append(zone)
    return roots


def iter_subtree(zone: dict):
    """Yield a zone and all of its nested descendants."""
    yield zone
    for child in zone["nested"]:
        yield from iter_subtree(child)


def find_targets(roots: list[dict], zone_name: str, src_file_contains: str) -> list[dict]:
    """Select target instances by zone name and src_file substring."""
    targets = []
    for root in roots:
        for zone in iter_subtree(root):
            if zone["name"] != zone_name:
                continue
            if src_file_contains and src_file_contains not in zone["src_file"]:
                continue
            targets.append(zone)
    targets.sort(key=lambda z: z["start_ns"])
    return targets


def deepest_zone_for_time(zone: dict, time_ns: int) -> dict | None:
    """Return the deepest nested zone (within ``zone``) containing ``time_ns``.

    Assumes ``time_ns`` lies within [zone.start, zone.end].
    """
    current = zone
    while True:
        nested = current["nested"]
        if not nested:
            return current
        starts = [z["start_ns"] for z in nested]
        idx = bisect.bisect_right(starts, time_ns) - 1
        if idx < 0:
            return current
        candidate = nested[idx]
        if candidate["start_ns"] <= time_ns <= candidate["end_ns"]:
            current = candidate
        else:
            return current


def attach_messages(host_roots: list[dict], messages: list[dict]) -> None:
    """Attach each message to the deepest enclosing zone among ``host_roots``.

    Messages carry only a timestamp (no thread), so we use time containment.
    Roots are grouped by thread (top-level zones on a single thread are
    sequential and non-overlapping, so a binary search finds the right one).
    When candidate roots on different threads both contain the timestamp, the
    tightest window (smallest duration) wins as the most specific host.
    """
    if not host_roots or not messages:
        return

    by_thread: dict[str, list[dict]] = {}
    for root in host_roots:
        by_thread.setdefault(root["thread"], []).append(root)
    for thread_roots in by_thread.values():
        thread_roots.sort(key=lambda z: z["start_ns"])
    thread_starts = {thread: [z["start_ns"] for z in roots] for thread, roots in by_thread.items()}

    for message in messages:
        time_ns = message["time_ns"]
        best = None
        for thread, thread_roots in by_thread.items():
            starts = thread_starts[thread]
            idx = bisect.bisect_right(starts, time_ns) - 1
            if idx < 0:
                continue
            root = thread_roots[idx]
            if not (root["start_ns"] <= time_ns <= root["end_ns"]):
                continue
            host = deepest_zone_for_time(root, time_ns)
            if best is None or host["duration_ns"] < best["duration_ns"]:
                best = host
        if best is not None:
            best["messages"].append(
                {"time_ns": time_ns, "rel_ns": time_ns - best["start_ns"], "text": message["text"]}
            )


def serialize_zone(zone: dict) -> dict:
    """Recursively convert a zone dict into its JSON representation."""
    result = {
        "name": zone["name"],
        "src_file": zone["src_file"],
        "src_line": zone["src_line"],
        "zone_name": zone["zone_name"],
        "zone_text": zone["zone_text"],
        "thread": zone["thread"],
        "start_ns": zone["start_ns"],
        "end_ns": zone["end_ns"],
        "duration_ns": zone["duration_ns"],
        "messages": sorted(zone["messages"], key=lambda m: m["time_ns"]),
        "nested": [serialize_zone(child) for child in zone["nested"]],
    }
    return result


def format_duration(ns: int) -> str:
    """Render a nanosecond duration in the most readable unit (e.g. 12.5ms, 902us)."""
    if ns >= 1_000_000_000:
        value, unit = ns / 1e9, "s"
    elif ns >= 1_000_000:
        value, unit = ns / 1e6, "ms"
    elif ns >= 1_000:
        value, unit = ns / 1e3, "us"
    else:
        return f"{ns}ns"
    text = f"{value:.1f}"
    if text.endswith(".0"):
        text = text[:-2]
    return f"{text}{unit}"


def render_zone_lines(zone: dict, depth: int, lines: list[str]) -> None:
    """Append indented text lines for ``zone`` and its nested zones / messages.

    Nested zones and messages are interleaved by time so the output mirrors the
    order in which events occurred during the zone's execution.
    """

    def fmt_file(file: str):
        return str(Path(file).resolve())

    indent = "  " * depth
    lines.append(f"{indent}{zone['name']} {format_duration(zone['duration_ns'])} ({fmt_file(zone['src_file'])}:{zone['src_line']})")

    # Interleave nested zones and messages by their timeline position.
    items = [("zone", z, z["start_ns"]) for z in zone["nested"]]
    items += [("message", m, m["time_ns"]) for m in zone["messages"]]
    items.sort(key=lambda item: item[2])

    child_indent = "  " * (depth + 1)
    for kind, obj, _ in items:
        if kind == "zone":
            render_zone_lines(obj, depth + 1, lines)
        else:
            text = obj.get("text", "").replace("\n", " ").strip()
            location = ""
            if obj.get("src_file"):
                location = f" ({fmt_file(obj['src_file'])}:{obj.get('src_line', '')})"
            lines.append(f"{child_indent}{text}{location}")


def render_text(targets: list[dict]) -> str:
    """Render all target instances as an indented human-readable tree."""
    blocks = []
    for target in targets:
        lines: list[str] = []
        render_zone_lines(target, 0, lines)
        blocks.append("\n".join(lines))
    return "\n\n".join(blocks)


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Extract a nested zone tree (with messages) from a Tracy capture as JSON.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument("tracy_file", help="Path to the .tracy capture file.")
    parser.add_argument(
        "--zone-name",
        default=DEFAULT_ZONE_NAME,
        help="Exact zone name to extract as target instances.",
    )
    parser.add_argument(
        "--src-file-contains",
        default=DEFAULT_SRC_FILE_CONTAINS,
        help="Only match target zones whose src_file contains this substring (empty disables).",
    )
    parser.add_argument(
        "--csvexport",
        default=None,
        help="Path to the tracy-csvexport binary (auto-resolved if omitted).",
    )
    parser.add_argument(
        "--output",
        default=None,
        help="Write output here instead of stdout.",
    )
    parser.add_argument(
        "--format",
        choices=("json", "text"),
        default="json",
        help="Output format: machine-readable JSON or an indented human-readable tree.",
    )
    parser.add_argument(
        "--all-messages",
        action="store_true",
        help="Attach messages across the whole capture instead of only target subtrees.",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)

    tracy_file = args.tracy_file
    if not Path(tracy_file).is_file():
        print(f"Error: tracy file not found: {tracy_file}", file=sys.stderr)
        return 1

    csvexport = resolve_csvexport(args.csvexport)

    with tempfile.TemporaryDirectory(prefix="tracy_zone_tree_") as tmp_dir:
        unwrap_csv = Path(tmp_dir) / "zones.csv"
        messages_csv = Path(tmp_dir) / "messages.csv"
        run_csvexport(csvexport, tracy_file, ["-u"], unwrap_csv)
        run_csvexport(csvexport, tracy_file, ["-m", "-s", ";"], messages_csv)

        zones = parse_zones(unwrap_csv)
        messages = parse_messages(messages_csv)

    roots = build_forest(zones)
    targets = find_targets(roots, args.zone_name, args.src_file_contains)

    if args.all_messages:
        # Attach against every root so messages outside target subtrees still land.
        attach_messages(roots, messages)
    else:
        attach_messages(targets, messages)

    if args.format == "text":
        payload = render_text(targets)
    else:
        payload = json.dumps([serialize_zone(target) for target in targets], indent=2)

    if args.output:
        Path(args.output).write_text(payload + "\n")
        print(
            f"Wrote {len(targets)} '{args.zone_name}' instance(s) to {args.output}",
            file=sys.stderr,
        )
    else:
        print(payload)

    return 0


if __name__ == "__main__":
    sys.exit(main())
