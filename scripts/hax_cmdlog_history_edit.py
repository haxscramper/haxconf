#!/usr/bin/env python
import json
import sys
import time
from collections import OrderedDict
from pathlib import Path

ARCHIVE_WHEN_BASE = 946684800


def iter_fish_history_commands(path: Path):
    lines = path.read_text().splitlines()

    i = 0
    while i < len(lines):
        line = lines[i]
        if not line.startswith("- cmd: "):
            i += 1
            continue

        cmd = line[len("- cmd: "):]
        i += 1

        while i < len(lines):
            line = lines[i]
            if line.startswith("- cmd: "):
                break
            if line.startswith("  when: ") or line.startswith("  paths: "):
                i += 1
                continue
            if line.startswith("  - "):
                i += 1
                continue
            if line.startswith("  "):
                cmd += "\n" + line[2:]
                i += 1
                continue
            break

        yield cmd


def iter_fish_history_entries(path: Path):
    lines = path.read_text().splitlines()

    i = 0
    while i < len(lines):
        line = lines[i]
        if not line.startswith("- cmd: "):
            i += 1
            continue

        cmd = line[len("- cmd: "):]
        when = None
        i += 1

        while i < len(lines):
            line = lines[i]
            if line.startswith("- cmd: "):
                break
            if line.startswith("  when: "):
                when = int(line[len("  when: "):])
                i += 1
                continue
            if line.startswith("  paths: "):
                i += 1
                continue
            if line.startswith("  - "):
                i += 1
                continue
            if line.startswith("  "):
                cmd += "\n" + line[2:]
                i += 1
                continue
            break

        yield {"command": cmd, "when": when}


def write_archive_from_fish_history(history_path: Path, archive_path: Path):
    counts = OrderedDict()

    for entry in iter_fish_history_entries(history_path):
        when = entry["when"]
        if when is not None and when < ARCHIVE_WHEN_BASE:
            continue

        cmd = entry["command"]
        if not cmd:
            continue

        counts[cmd] = counts.get(cmd, 0) + 1

    with archive_path.open("w") as out:
        for command, count in counts.items():
            json.dump({"command": command, "count": count}, out, separators=(",", ":"))
            out.write("\n")


def fish_escape_multiline(command: str) -> str:
    lines = command.split("\n")
    first = lines[0]
    rest = lines[1:]
    if not rest:
        return first
    return first + "\n" + "\n".join("  " + line for line in rest)


def write_fish_history_from_archive(archive_path: Path, history_path: Path):
    with archive_path.open() as infile:
        entries = []
        when = 1
        for line in infile:
            line = line.strip()
            if not line:
                continue
            obj = json.loads(line)
            command = obj["command"]
            count = int(obj.get("count", 1))
            for _ in range(count):
                entries.append((command, when))
                when += 1

    with history_path.open("w") as out:
        for command, when in entries:
            out.write(f"- cmd: {fish_escape_multiline(command)}\n")
            out.write(f"  when: {when}\n")


def main() -> None:
    subcommand = sys.argv[1]

    if subcommand == "archive-fish-history":
        history_path = Path(sys.argv[2])
        archive_path = Path(sys.argv[3])
        write_archive_from_fish_history(history_path, archive_path)
        return

    if subcommand == "read-fish-history-archive":
        archive_path = Path(sys.argv[2])
        history_path = Path(sys.argv[3])
        write_fish_history_from_archive(archive_path, history_path)
        return

    raise SystemExit(f"unknown subcommand: {subcommand}")


if __name__ == "__main__":
    main()
