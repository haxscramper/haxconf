#!/usr/bin/env python

import json
import subprocess
import sys
import enum
from dataclasses import dataclass
from datetime import datetime, timezone
from html import escape
from typing import Any
from pathlib import Path

ROFI_THEME: str = """
* {
  font: "Iosevka 12";
}
window {
  width: 50%;
  height: 95%;
}
listview {
  lines: 20;
  columns: 1;
  fixed-height: false;
  dynamic: true;
}
element-text {
  markup: true;
}
"""

TODO_COLORS: dict[str, str] = {
    "TODO": "#e06c75",
    "NEXT": "#61afef",
    "WIP": "#98c379",
    "PAUSED": "#e5c07b",
    "BLOCKED": "#be5046",
    "DONE": "#56b6c2",
    "CANCELLED": "#5c6370",
}

PRIORITY_COLORS: dict[str, str] = {
    "A": "#ff6b6b",
    "B": "#e5c07b",
    "C": "#98c379",
}

TAG_COLOR: str = "#6c7086"
DEFAULT_TODO_COLOR: str = "#abb2bf"
DEFAULT_PRIORITY_COLOR: str = "#7f848e"
HEADER_COLOR: str = "#89b4fa"
GROUP_COLOR: str = "#cba6f7"
MUTED_COLOR: str = "#9399b2"


@dataclass(frozen=True)
class ColumnWidths:
    file: int
    age: int
    last_clocked: int
    overall_time: int
    todo: int
    priority: int


@dataclass(frozen=True)
class DisplayItem:
    display: str
    entry: dict[str, Any] | None
    selectable: bool


def color_for_todo(state: str | None) -> str:
    return TODO_COLORS.get(state or "", DEFAULT_TODO_COLOR)


def color_for_priority(priority: str | None) -> str:
    return PRIORITY_COLORS.get(priority or "", DEFAULT_PRIORITY_COLOR)


def make_cell(
    text: str | None,
    width: int,
    align: str,
    color: str | None,
    bold: bool,
) -> str:
    content_text: str = "" if text is None else str(text)

    if align == "center":
        padded: str = f"{content_text:^{width}}"
    elif align == "right":
        padded = f"{content_text:>{width}}"
    else:
        padded = f"{content_text:<{width}}"

    attrs: list[str] = []
    if color is not None:
        attrs.append(f'foreground="{color}"')
    if bold:
        attrs.append('weight="bold"')

    attr_text: str = ""
    if attrs:
        attr_text = " " + " ".join(attrs)

    return f"<span{attr_text}>{escape(padded)}</span>"


def parse_timestamp(timestamp: str | None) -> datetime | None:
    if not timestamp:
        return None
    return datetime.strptime(timestamp, "%Y-%m-%dT%H:%M:%S%z")


def format_relative_time(timestamp: str | None, now: datetime) -> str:
    dt: datetime | None = parse_timestamp(timestamp)
    if dt is None:
        return ""

    total_seconds: int = max(0, int((now - dt).total_seconds()))
    minutes_total, _ = divmod(total_seconds, 60)
    hours_total, minutes = divmod(minutes_total, 60)
    days_total, hours = divmod(hours_total, 24)

    years, rem_days = divmod(days_total, 365)
    months, days = divmod(rem_days, 30)

    parts: list[str] = []
    units: list[tuple[int, str]] = [
        (years, "y"),
        (months, "m"),
        (days, "d"),
        (hours, "h"),
        (minutes, "m"),
    ]

    for value, suffix in units:
        if value != 0:
            parts.append(f"{value}{suffix}")
        if len(parts) == 2:
            break

    if not parts:
        return "0m"

    return " ".join(parts)


def format_overall_time(minutes_total: int | None) -> str:
    total: int = minutes_total or 0
    hours, minutes = divmod(total, 60)
    if hours == 0 and minutes == 0:
        return ""

    else:
        return f"{hours:02}:{minutes:02}"


def normalize_tags(entry: dict[str, Any]) -> str:
    tags: list[str] = entry.get("subtree-tags") or []
    return " ".join(tags)


def normalize_todo(entry: dict[str, Any]) -> str:
    return entry.get("todo-state") or ""


def normalize_priority(entry: dict[str, Any]) -> str:
    return entry.get("subtree-priority") or ""


def normalize_title(entry: dict[str, Any]) -> str:
    return entry.get("title") or ""


def normalize_path(entry: dict[str, Any]) -> str:
    return Path(entry.get("file", "")).name


def normalize_created_age(entry: dict[str, Any], now: datetime) -> str:
    created: dict[str, Any] | None = entry.get("created")
    if not created:
        return ""
    return format_relative_time(created.get("timestamp"), now)


def normalize_last_clocked(entry: dict[str, Any], now: datetime) -> str:
    return format_relative_time(entry.get("last-clocked-in"), now)


def normalize_overall_time(entry: dict[str, Any]) -> str:
    return format_overall_time(entry.get("overall-time"))


class Columns(str, enum.Enum):
    FILE = "path"
    CREATED = "CREATED"
    LAST_CLOCKED = "L-CLOCKED"
    OVERALL_CLOCKED = "TIME"
    TODO_STATE = "TODO"
    TITLE = "Title"
    TAGS = "TAGS"
    PRIORITY = "[#]"


def infer_widths(groups: list[dict[str, Any]], now: datetime) -> ColumnWidths:
    entries: list[dict[str, Any]] = []
    for group in groups:
        entries.extend(group.get("entries") or [])

    return ColumnWidths(
        file=max([len(Columns.FILE.value)] +
                 [len(normalize_path(entry)) for entry in entries]),
        age=max([len(Columns.CREATED.value)] +
                [len(normalize_created_age(entry, now)) for entry in entries]),
        last_clocked=max(
            [len(Columns.LAST_CLOCKED.value)] +
            [len(normalize_last_clocked(entry, now)) for entry in entries]),
        overall_time=max(
            [len(Columns.OVERALL_CLOCKED.value)] +
            [len(normalize_overall_time(entry)) for entry in entries]),
        todo=max([len(Columns.TODO_STATE.value)] +
                 [len(normalize_todo(entry)) for entry in entries]),
        priority=max([len(Columns.PRIORITY.value)] +
                     [len(normalize_priority(entry)) for entry in entries]),
    )


def format_header(widths: ColumnWidths) -> str:
    return " ".join([
        make_cell(Columns.FILE.value, widths.file, "right", HEADER_COLOR,
                  True),
        make_cell(Columns.CREATED.value, widths.age, "right", HEADER_COLOR,
                  True),
        make_cell(Columns.LAST_CLOCKED.value, widths.last_clocked, "right",
                  HEADER_COLOR, True),
        make_cell(Columns.OVERALL_CLOCKED.value, widths.overall_time, "right",
                  HEADER_COLOR, True),
        make_cell(Columns.TODO_STATE.value, widths.todo, "center",
                  HEADER_COLOR, True),
        make_cell(Columns.PRIORITY.value, widths.priority, "center",
                  HEADER_COLOR, True),
    ])


def format_group_header(header: str, widths: ColumnWidths) -> str:
    return f'<span foreground="{GROUP_COLOR}" weight="bold">{escape(header)}</span>'


def format_entry(entry: dict[str, Any], widths: ColumnWidths,
                 now: datetime) -> str:
    title: str = normalize_title(entry)
    tags: str = normalize_tags(entry)

    title_and_tags_cell: str = f"<span>{escape(title)}</span>"
    if tags:
        title_and_tags_cell += f' <span foreground="{TAG_COLOR}">{escape(tags)}</span>'

    return " ".join([
        make_cell(
            text=normalize_path(entry),
            width=widths.file,
            align="right",
            color=MUTED_COLOR,
            bold=False,
        ),
        make_cell(
            text=normalize_created_age(entry, now),
            width=widths.age,
            align="right",
            color=MUTED_COLOR,
            bold=False,
        ),
        make_cell(
            text=normalize_last_clocked(entry, now),
            width=widths.last_clocked,
            align="right",
            color=MUTED_COLOR,
            bold=False,
        ),
        make_cell(
            text=normalize_overall_time(entry),
            width=widths.overall_time,
            align="right",
            color=MUTED_COLOR,
            bold=False,
        ),
        make_cell(
            text=normalize_todo(entry),
            width=widths.todo,
            align="center",
            color=color_for_todo(normalize_todo(entry)),
            bold=True,
        ),
        make_cell(
            text=normalize_priority(entry),
            width=widths.priority,
            align="center",
            color=color_for_priority(normalize_priority(entry)),
            bold=True,
        ), title_and_tags_cell
    ])


def build_display_items(groups: list[dict[str, Any]]) -> list[DisplayItem]:
    now: datetime = datetime.now().astimezone()
    widths: ColumnWidths = infer_widths(groups, now)

    items: list[DisplayItem] = []
    items.append(DisplayItem(display="", entry=None, selectable=False))
    items.append(
        DisplayItem(display=format_header(widths),
                    entry=None,
                    selectable=False))

    for group in groups:
        header: str = group.get("header") or ""
        items.append(
            DisplayItem(
                display=format_group_header(header, widths),
                entry=None,
                selectable=False,
            ))

        def get_tmp_time(it: dict, field: str):
            value = it.get(field)
            if isinstance(value, dict) and "timestamp" in value:
                value = value.get("timestamp")

            if value:
                dt = datetime.fromisoformat(value)
                if dt.tzinfo is None:
                    return dt.replace(tzinfo=timezone.utc)
                return dt.astimezone(timezone.utc)

        def relevant_time(it: dict):
            if res := get_tmp_time(it, "last-clocked-in"):
                return res
            if res := get_tmp_time(it, "created"):
                return res
            else:
                return datetime.fromtimestamp(0, tz=timezone.utc)

        for entry in sorted(
                group.get("entries") or [],
                key=relevant_time,
                reverse=True,
        ):
            items.append(
                DisplayItem(
                    display=format_entry(entry, widths, now),
                    entry=entry,
                    selectable=True,
                ))

    return items


def run_rofi(items: list[DisplayItem]) -> dict[str, Any] | None:
    rofi_input: str = "\n".join(item.display for item in items)
    nonselectable_rows: list[str] = [
        str(index) for index, item in enumerate(items) if not item.selectable
    ]

    proc: subprocess.CompletedProcess[str] = subprocess.run(
        [
            "rofi",
            "-dmenu",
            "-markup-rows",
            "-i",
            "-p",
            "Agenda",
            "-theme-str",
            ROFI_THEME,
            "-format",
            "i",
            "-a",
            ",".join(nonselectable_rows),
        ],
        input=rofi_input,
        text=True,
        capture_output=True,
        check=False,
    )

    if proc.returncode != 0:
        sys.exit(proc.returncode)

    selected_index_text: str = proc.stdout.strip()
    if not selected_index_text:
        return None

    selected_index: int = int(selected_index_text)
    if selected_index < 0 or selected_index >= len(items):
        return None

    selected_item: DisplayItem = items[selected_index]
    if not selected_item.selectable:
        return None

    return selected_item.entry


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit(1)

    input_path: str = sys.argv[1]

    with open(input_path, "r", encoding="utf-8") as infile:
        groups: list[dict[str, Any]] = json.load(infile)

    has_entries: bool = any(group.get("entries") for group in groups)
    if not has_entries:
        raise SystemExit(0)

    items: list[DisplayItem] = build_display_items(groups)
    selected: dict[str, Any] | None = run_rofi(items)

    if selected is None:
        return

    print(json.dumps(selected))


if __name__ == "__main__":
    main()
