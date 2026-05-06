#!/usr/bin/env python

import json
import subprocess
import sys
from dataclasses import dataclass
from html import escape
from typing import Any

ROFI_THEME: str = """
* {
  font: "Iosevka 12";
}
window {
  width: 50%;
  height: 80%;
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


@dataclass(frozen=True)
class ColumnWidths:
    todo: int
    priority: int
    title: int
    tags: int


@dataclass(frozen=True)
class DisplayItem:
    display: str
    entry: dict[str, Any]


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


def normalize_tags(entry: dict[str, Any]) -> str:
    tags: list[str] = entry.get("subtree-tags") or []
    return " ".join(tags)


def normalize_todo(entry: dict[str, Any]) -> str:
    return entry.get("todo-state") or ""


def normalize_priority(entry: dict[str, Any]) -> str:
    return entry.get("subtree-priority") or ""


def normalize_title(entry: dict[str, Any]) -> str:
    return entry.get("title") or ""


def infer_widths(entries: list[dict[str, Any]]) -> ColumnWidths:
    todo_width: int = max(len(normalize_todo(entry)) for entry in entries)
    priority_width: int = max(
        len(normalize_priority(entry)) for entry in entries)
    title_width: int = max(len(normalize_title(entry)) for entry in entries)
    tags_width: int = max(len(normalize_tags(entry)) for entry in entries)

    return ColumnWidths(
        todo=todo_width,
        priority=priority_width,
        title=title_width,
        tags=tags_width,
    )


def format_entry(entry: dict[str, Any], widths: ColumnWidths) -> str:
    todo: str = normalize_todo(entry)
    priority: str = normalize_priority(entry)
    title: str = normalize_title(entry)
    tags: str = normalize_tags(entry)

    todo_cell: str = make_cell(
        text=todo,
        width=widths.todo,
        align="center",
        color=color_for_todo(todo),
        bold=True,
    )
    priority_cell: str = make_cell(
        text=priority,
        width=widths.priority,
        align="center",
        color=color_for_priority(priority),
        bold=True,
    )
    title_cell: str = make_cell(
        text=title,
        width=widths.title,
        align="left",
        color=None,
        bold=False,
    )
    tags_cell: str = make_cell(
        text=tags,
        width=widths.tags,
        align="right",
        color=TAG_COLOR,
        bold=False,
    )

    return f"{todo_cell} {priority_cell} {title_cell} {tags_cell}"


def flatten_entries(groups: list[dict[str, Any]]) -> list[dict[str, Any]]:
    result: list[dict[str, Any]] = []
    for group in groups:
        entries: list[dict[str, Any]] = group.get("entries") or []
        result.extend(entries)
    return result


def build_display_items(entries: list[dict[str, Any]]) -> list[DisplayItem]:
    widths: ColumnWidths = infer_widths(entries)
    return [
        DisplayItem(display=format_entry(entry, widths), entry=entry)
        for entry in entries
    ]


def run_rofi(items: list[DisplayItem]) -> dict[str, Any] | None:
    rofi_input: str = "\n".join(item.display for item in items)

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
        ],
        input=rofi_input,
        text=True,
        capture_output=True,
        check=False,
    )

    if proc.returncode != 0:
        sys.exit(proc.returncode)

    selected: str = proc.stdout.rstrip("\n")
    if not selected:
        return None

    for item in items:
        if item.display == selected:
            return item.entry

    return None


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit(1)

    input_path: str = sys.argv[1]

    with open(input_path, "r", encoding="utf-8") as infile:
        groups: list[dict[str, Any]] = json.load(infile)

    entries: list[dict[str, Any]] = flatten_entries(groups)
    if not entries:
        raise SystemExit(0)

    items: list[DisplayItem] = build_display_items(entries)
    selected: dict[str, Any] | None = run_rofi(items)

    if selected is None:
        return

    print(json.dumps(selected))


if __name__ == "__main__":
    main()
