#!/usr/bin/env python
# /// script
# dependencies = [
#   "pydantic",
# ]
# ///

import enum
import json
import math
import re
import subprocess
import sys
from dataclasses import dataclass
from datetime import datetime, timedelta
from html import escape
from pathlib import Path

from pydantic import BaseModel, ConfigDict, Field, TypeAdapter, field_validator

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
    "A": "#F0DFAF",
    "B": "#FD971F",
    "C": "#66D9EF",
    "D": "#A1EFE4",
    "E": "#A6E22E",
    "F": "#AE81FF",
    "S": "#FD5FF0",
    "X": "#FF3131",
}

TAG_COLOR: str = "#6c7086"
DEFAULT_TODO_COLOR: str = "#abb2bf"
DEFAULT_PRIORITY_COLOR: str = "#7f848e"
HEADER_COLOR: str = "#89b4fa"
GROUP_COLOR: str = "#cba6f7"
MUTED_COLOR: str = "#9399b2"

OFFSET_RED: str = "#f38ba8"
OFFSET_ORANGE: str = "#fab387"
OFFSET_YELLOW: str = "#f9e2af"
OFFSET_GREEN_BRIGHT: str = "#a6e3a1"
OFFSET_GREEN_MEDIUM: str = "#74c77f"
OFFSET_GREEN_DARK: str = "#4da561"
OFFSET_CYAN: str = "#89dceb"

FRACTION_SYMBOLS: list[tuple[float, str]] = [
    (1.0, "⅟"),
    (0.0, "⁰"),
    (0.25, "¼"),
    (0.5, "½"),
    (0.75, "¾"),
    (0.33, "⅓"),
    (0.66, "⅔"),
    (0.2, "⅕"),
    (0.4, "⅖"),
    (0.6, "⅗"),
    (0.8, "⅘"),
    (0.16, "⅙"),
    (0.83, "⅚"),
]


def parse_timestamp(value: str | datetime | None) -> datetime | None:
    if value is None or value == "":
        return None
    if isinstance(value, datetime):
        return value
    return datetime.strptime(value, "%Y-%m-%dT%H:%M:%S%z")


def format_relative_time(dt: datetime | None, now: datetime) -> str:
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
    return f"{hours:02}:{minutes:02}"


def repeat_to_timedelta(repeat: str) -> timedelta:
    match = re.fullmatch(r"\+\+(\d+)([hdwmy])", repeat)
    if match is None:
        raise ValueError(f"Unsupported repeat format: {repeat}")

    amount = int(match.group(1))
    unit = match.group(2)

    if unit == "h":
        return timedelta(hours=amount)
    if unit == "d":
        return timedelta(days=amount)
    if unit == "w":
        return timedelta(weeks=amount)
    if unit == "m":
        return timedelta(days=30 * amount)
    if unit == "y":
        return timedelta(days=365 * amount)

    raise ValueError(f"Unsupported repeat unit: {unit}")


def format_offset(delta_hours: float) -> str:
    sign = "-" if delta_hours < 0 else "+"
    absolute_hours = abs(delta_hours)

    if absolute_hours >= 24 * 7:
        weeks = math.floor(absolute_hours / (24 * 7))
        return f"{sign}{weeks}w"

    whole = math.floor(absolute_hours)
    frac = absolute_hours - whole
    symbol = min(FRACTION_SYMBOLS, key=lambda it: abs(it[0] - frac))[1]
    return f"{sign}{whole}{symbol}"


def color_for_offset(due: datetime | None, now: datetime) -> str:
    if due is None:
        return MUTED_COLOR

    due_local = due.astimezone(now.tzinfo)
    delta_seconds = (due_local - now).total_seconds()
    end_of_today = now.replace(hour=23,
                               minute=59,
                               second=59,
                               microsecond=999999)

    if delta_seconds < -24 * 3600:
        return OFFSET_RED
    if delta_seconds < 0:
        return OFFSET_ORANGE
    if due_local <= end_of_today:
        return OFFSET_YELLOW
    if delta_seconds <= 24 * 3600:
        return OFFSET_GREEN_BRIGHT
    if delta_seconds <= 48 * 3600:
        return OFFSET_GREEN_MEDIUM
    if delta_seconds <= 72 * 3600:
        return OFFSET_GREEN_DARK
    return OFFSET_CYAN


class TimestampWithRepeat(BaseModel):
    timestamp: datetime | None = None
    repeat: str | None = None

    @field_validator("timestamp", mode="before")
    @classmethod
    def _parse_timestamp(cls, value: str | datetime | None) -> datetime | None:
        return parse_timestamp(value)


class AgendaEntry(BaseModel):
    model_config = ConfigDict(populate_by_name=True)

    title: str | None = None
    created: TimestampWithRepeat | None = None
    deadline: TimestampWithRepeat | None = None
    scheduled: TimestampWithRepeat | None = None
    effort: int | None = None
    last_repeat: TimestampWithRepeat | None = Field(default=None,
                                                    alias="last-repeat")
    last_clocked_in: datetime | None = Field(default=None,
                                             alias="last-clocked-in")
    overall_time: int | None = Field(default=None, alias="overall-time")
    todo_state: str | None = Field(default=None, alias="todo-state")
    subtree_priority: str | None = Field(default=None,
                                         alias="subtree-priority")
    subtree_tags: list[str] | None = Field(default=None, alias="subtree-tags")
    file: str = ""
    subtree_path: list[str] | None = Field(default=None, alias="subtree-path")
    subtree_line: int | None = Field(default=None, alias="subtree-line")

    @field_validator("last_clocked_in", mode="before")
    @classmethod
    def _parse_last_clocked(cls,
                            value: str | datetime | None) -> datetime | None:
        return parse_timestamp(value)

    @field_validator("effort", mode="before")
    @classmethod
    def _parse_effort(cls, value: str | None) -> datetime | None:
        if value:
            hours, minutes = value.split(":")
            return int(hours) * 60 + int(minutes)

        else:
            return None

    def normalized_tags(self) -> str:
        return " ".join(self.subtree_tags or [])

    def normalized_todo(self) -> str:
        return self.todo_state or ""

    def normalized_priority(self) -> str:
        return self.subtree_priority or ""

    def normalized_effort_int(self) -> int:
        return self.effort or 0

    def normalized_effort(self) -> str:
        if self.effort:
            return f"{self.effort // 60}:{self.effort % 60}"

        else:
            return ""

    def normalized_title(self) -> str:
        return self.title or ""

    def normalized_path(self) -> str:
        return Path(self.file).name

    def normalized_created_age(self, now: datetime) -> str:
        created_timestamp = self.created.timestamp if self.created else None
        return format_relative_time(created_timestamp, now)

    def normalized_last_clocked(self, now: datetime) -> str:
        return format_relative_time(self.last_clocked_in, now)

    def normalized_overall_time(self) -> str:
        return format_overall_time(self.overall_time)

    def effective_scheduled(self) -> datetime | None:
        if self.scheduled is None or self.scheduled.timestamp is None:
            if self.deadline and self.deadline.timestamp:
                return self.deadline.timestamp

            else:
                return None

        if not self.scheduled.repeat:
            return self.scheduled.timestamp

        interval = repeat_to_timedelta(self.scheduled.repeat)
        anchor = (self.last_repeat.timestamp if self.last_repeat
                  and self.last_repeat.timestamp else self.scheduled.timestamp)
        return anchor + interval

    def normalized_offset(self, now: datetime) -> str:
        due = self.effective_scheduled()
        if due is None:
            return ""
        due_local = due.astimezone(now.tzinfo)
        delta_hours = (due_local - now).total_seconds() / 3600.0
        return format_offset(delta_hours)

    def sort_reference_time(self) -> datetime:
        if self.last_clocked_in is not None:
            return self.last_clocked_in
        if self.created and self.created.timestamp:
            return self.created.timestamp
        return datetime.fromtimestamp(0, tz=now_utc())

    def is_due_today_or_overdue(self, now: datetime) -> bool:
        due = self.effective_scheduled()
        if due is None:
            return False
        due_local = due.astimezone(now.tzinfo)
        end_of_today = now.replace(hour=23,
                                   minute=59,
                                   second=59,
                                   microsecond=999999)
        return due_local <= end_of_today


class AgendaGroup(BaseModel):
    header: str | None = None
    entries: list[AgendaEntry] = Field(default_factory=list)


@dataclass(frozen=True)
class ColumnWidths:
    created: int
    file: int
    offset: int
    last_clocked: int
    overall_time: int
    todo: int
    priority: int
    effort: int


@dataclass(frozen=True)
class DisplayItem:
    display: str
    entry: AgendaEntry | None
    selectable: bool


class Columns(str, enum.Enum):
    CREATED = "CREATED"
    FILE = "path"
    OFFSET = "ΔSCH"
    LAST_CLOCKED = "L-CLOCKED"
    OVERALL_CLOCKED = "TIME"
    TODO_STATE = "TODO"
    PRIORITY = "[#]"
    EFFORT = "EEE"


def now_utc() -> datetime:
    return datetime.now().astimezone().astimezone()


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


def infer_widths(groups: list[AgendaGroup], now: datetime) -> ColumnWidths:
    entries: list[AgendaEntry] = []
    for group in groups:
        entries.extend(group.entries)

    return ColumnWidths(
        created=max(
            [len(Columns.CREATED.value)] +
            [len(entry.normalized_created_age(now)) for entry in entries]),
        file=max([len(Columns.FILE.value)] +
                 [len(entry.normalized_path()) for entry in entries]),
        offset=max([len(Columns.OFFSET.value)] +
                   [len(entry.normalized_offset(now)) for entry in entries]),
        last_clocked=max(
            [len(Columns.LAST_CLOCKED.value)] +
            [len(entry.normalized_last_clocked(now)) for entry in entries]),
        overall_time=max(
            [len(Columns.OVERALL_CLOCKED.value)] +
            [len(entry.normalized_overall_time()) for entry in entries]),
        todo=max([len(Columns.TODO_STATE.value)] +
                 [len(entry.normalized_todo()) for entry in entries]),
        priority=max([len(Columns.PRIORITY.value)] +
                     [len(entry.normalized_priority()) for entry in entries]),
        effort=max([len(Columns.EFFORT.value)] +
                   [len(entry.normalized_effort()) for entry in entries]),
    )


def format_header(widths: ColumnWidths) -> str:
    return " ".join([
        make_cell(Columns.CREATED.value, widths.created, "right", HEADER_COLOR,
                  True),
        make_cell(Columns.FILE.value, widths.file, "right", HEADER_COLOR,
                  True),
        make_cell(Columns.OFFSET.value, widths.offset, "right", HEADER_COLOR,
                  True),
        make_cell(Columns.LAST_CLOCKED.value, widths.last_clocked, "right",
                  HEADER_COLOR, True),
        make_cell(Columns.OVERALL_CLOCKED.value, widths.overall_time, "right",
                  HEADER_COLOR, True),
        make_cell(Columns.TODO_STATE.value, widths.todo, "center",
                  HEADER_COLOR, True),
        make_cell(Columns.PRIORITY.value, widths.priority, "center",
                  HEADER_COLOR, True),
        make_cell(Columns.EFFORT.value, widths.effort, "center", HEADER_COLOR,
                  True),
    ])


def format_group_header(header: str) -> str:
    return f'<span foreground="{GROUP_COLOR}" weight="bold">{escape(header)}</span>'


def format_entry(entry: AgendaEntry, widths: ColumnWidths,
                 now: datetime) -> str:
    title = entry.normalized_title()
    tags = entry.normalized_tags()
    due = entry.effective_scheduled()

    title_and_tags_cell: str = f"<span>{escape(title)}</span>"
    if tags:
        title_and_tags_cell += f' <span foreground="{TAG_COLOR}">{escape(tags)}</span>'

    result = " ".join([
        make_cell(
            text=entry.normalized_created_age(now),
            width=widths.created,
            align="right",
            color=MUTED_COLOR,
            bold=False,
        ),
        make_cell(
            text=entry.normalized_path(),
            width=widths.file,
            align="right",
            color=MUTED_COLOR,
            bold=False,
        ),
        make_cell(
            text=entry.normalized_offset(now),
            width=widths.offset,
            align="right",
            color=color_for_offset(due, now),
            bold=True,
        ),
        make_cell(
            text=entry.normalized_last_clocked(now),
            width=widths.last_clocked,
            align="right",
            color=MUTED_COLOR,
            bold=False,
        ),
        make_cell(
            text=entry.normalized_overall_time(),
            width=widths.overall_time,
            align="right",
            color=MUTED_COLOR,
            bold=False,
        ),
        make_cell(
            text=entry.normalized_todo(),
            width=widths.todo,
            align="center",
            color=color_for_todo(entry.normalized_todo()),
            bold=True,
        ),
        make_cell(
            text=entry.normalized_priority(),
            width=widths.priority,
            align="center",
            color=color_for_priority(entry.normalized_priority()),
            bold=True,
        ),
        make_cell(
            text=entry.normalized_effort(),
            width=widths.effort,
            align="center",
            color=MUTED_COLOR,
            bold=True,
        ),
        title_and_tags_cell,
    ])

    match entry.normalized_priority():
        case "X" | "S":
            result = f'<span background="{PRIORITY_COLORS[entry.normalized_priority()]}66">{result}</span>'

    return result


def build_display_items(groups: list[AgendaGroup]) -> list[DisplayItem]:
    now = datetime.now().astimezone()
    widths = infer_widths(groups, now)

    items: list[DisplayItem] = []
    items.append(DisplayItem(display="", entry=None, selectable=False))
    items.append(
        DisplayItem(display=format_header(widths),
                    entry=None,
                    selectable=False))

    for group in groups:
        items.append(
            DisplayItem(
                display=format_group_header(group.header or ""),
                entry=None,
                selectable=False,
            ))

        def sort_key(entry: AgendaEntry) -> tuple[int, int, int, int, float]:
            match entry.subtree_priority:
                case "X":
                    priority_rank = 0
                case "S":
                    priority_rank = 1
                case _:
                    priority_rank = 2

            match entry.todo_state:
                case "WIP":
                    todo_rank = 0
                case "TODO":
                    todo_rank = 1
                case "NEXT":
                    todo_rank = 2
                case "PAUSED":
                    todo_rank = 4
                case _:
                    todo_rank = 3

            due = entry.effective_scheduled()
            if due is not None:
                due_ts = due.astimezone(now.tzinfo).timestamp()
                if entry.is_due_today_or_overdue(now):
                    return (priority_rank, todo_rank, 0, entry.normalized_effort_int(), due_ts)

            reference_ts = entry.sort_reference_time().astimezone(
                now.tzinfo).timestamp()
            return (priority_rank, todo_rank, 1, entry.normalized_effort_int(), -reference_ts)

        sorted_entries = sorted(group.entries, key=sort_key)

        for entry in sorted_entries:
            items.append(
                DisplayItem(
                    display=format_entry(entry, widths, now),
                    entry=entry,
                    selectable=True,
                ))

    return items


def run_rofi(items: list[DisplayItem]) -> AgendaEntry | None:
    rofi_input: str = "\n".join(item.display for item in items)
    Path("/tmp/debug.txt").write_text(rofi_input)
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
            "-matching",
            "fuzzy",
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

    input_path = sys.argv[1]

    with open(input_path, "r", encoding="utf-8") as infile:
        raw_data = json.load(infile)

    groups = TypeAdapter(list[AgendaGroup]).validate_python(raw_data)

    has_entries = any(group.entries for group in groups)
    if not has_entries:
        raise SystemExit(0)

    items = build_display_items(groups)
    selected = run_rofi(items)

    if selected is None:
        return

    print(json.dumps(selected.model_dump(by_alias=True, mode="json")))


if __name__ == "__main__":
    main()
