#!/usr/bin/env python
# /// script
# dependencies = [
#   "PyQt6>=6.7",
#   "beartype>=0.19",
# ]
# ///

import argparse
import json
import sys
from dataclasses import dataclass
from pathlib import Path

from beartype import beartype
from beartype.typing import Any, List
from PyQt6.QtCore import QAbstractListModel, QModelIndex, QSortFilterProxyModel, Qt
from PyQt6.QtGui import QKeyEvent, QKeySequence, QShortcut
from PyQt6.QtWidgets import (
    QApplication,
    QHBoxLayout,
    QLineEdit,
    QListView,
    QMainWindow,
    QPlainTextEdit,
    QPushButton,
    QSplitter,
    QVBoxLayout,
    QWidget,
)


@beartype
@dataclass(frozen=True)
class CommandEntry:
    command: str
    count: int
    order: int


class CommandListModel(QAbstractListModel):
    command_role = int(Qt.ItemDataRole.UserRole) + 1
    count_role = int(Qt.ItemDataRole.UserRole) + 2
    order_role = int(Qt.ItemDataRole.UserRole) + 3

    @beartype
    def __init__(self, entries: List[CommandEntry]) -> None:
        super().__init__()
        self.entries = entries

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        if parent.isValid():
            return 0
        return len(self.entries)

    def data(self, index: QModelIndex, role: int = int(Qt.ItemDataRole.DisplayRole)) -> Any:
        if not index.isValid():
            return None
        row = index.row()
        if row < 0 or len(self.entries) <= row:
            return None

        entry = self.entries[row]
        match role:
            case int(Qt.ItemDataRole.DisplayRole):
                compact = entry.command.replace("\n", " ⏎ ")
                return f"{entry.count:>4}  {compact}"
            case self.command_role:
                return entry.command
            case self.count_role:
                return entry.count
            case self.order_role:
                return entry.order
            case _:
                return None


class CommandFilterProxyModel(QSortFilterProxyModel):
    @beartype
    def __init__(self) -> None:
        super().__init__()
        self.query = ""

    @beartype
    def set_query(self, query: str) -> None:
        self.query = query.casefold().strip()
        self.invalidateFilter()

    def filterAcceptsRow(self, source_row: int, source_parent: QModelIndex) -> bool:
        if source_parent.isValid():
            return False
        if self.query == "":
            return True

        source = self.sourceModel()
        if source is None:
            raise ValueError("Proxy model has no source model configured")
        index = source.index(source_row, 0)
        command = source.data(index, CommandListModel.command_role)
        if not isinstance(command, str):
            raise ValueError(f"Expected command string for row {source_row}, got {command!r}")
        return self.query in command.casefold()


class SearchLineEdit(QLineEdit):
    @beartype
    def __init__(self, command_view: QListView) -> None:
        super().__init__()
        self.command_view = command_view

    def keyPressEvent(self, event: QKeyEvent) -> None:
        match event.key():
            case Qt.Key.Key_Down | Qt.Key.Key_Up | Qt.Key.Key_PageDown | Qt.Key.Key_PageUp:
                QApplication.sendEvent(self.command_view, event)
                return
            case _:
                super().keyPressEvent(event)


class CommandSelectorWindow(QMainWindow):
    @beartype
    def __init__(self, entries: List[CommandEntry], source_path: Path) -> None:
        super().__init__()

        self.setWindowTitle(f"Command Selector: {source_path}")
        self.resize(1200, 700)

        self.source_model = CommandListModel(entries)
        self.proxy_model = CommandFilterProxyModel()
        self.proxy_model.setSourceModel(self.source_model)

        self.command_view = QListView()
        self.command_view.setModel(self.proxy_model)
        self.command_view.setAlternatingRowColors(True)

        self.search_input = SearchLineEdit(self.command_view)
        self.search_input.setPlaceholderText("Type filter text...")

        self.preview = QPlainTextEdit()
        self.preview.setReadOnly(True)
        self.preview.setLineWrapMode(QPlainTextEdit.LineWrapMode.WidgetWidth)

        self.ok_button = QPushButton("OK")
        self.ok_button.clicked.connect(self.copy_selected_command)

        self.copy_shortcut_return = QShortcut(QKeySequence("Ctrl+Return"), self)
        self.copy_shortcut_return.activated.connect(self.copy_selected_command)

        self.copy_shortcut_enter = QShortcut(QKeySequence("Ctrl+Enter"), self)
        self.copy_shortcut_enter.activated.connect(self.copy_selected_command)

        left_panel = QWidget()
        left_layout = QVBoxLayout(left_panel)
        left_layout.setContentsMargins(0, 0, 0, 0)
        left_layout.addWidget(self.search_input)
        left_layout.addWidget(self.command_view)

        right_panel = QWidget()
        right_layout = QVBoxLayout(right_panel)
        right_layout.setContentsMargins(0, 0, 0, 0)
        right_layout.addWidget(self.preview)
        right_layout.addWidget(self.ok_button)

        splitter = QSplitter(Qt.Orientation.Horizontal)
        splitter.addWidget(left_panel)
        splitter.addWidget(right_panel)
        splitter.setStretchFactor(0, 2)
        splitter.setStretchFactor(1, 3)

        root = QWidget()
        root_layout = QHBoxLayout(root)
        root_layout.setContentsMargins(8, 8, 8, 8)
        root_layout.addWidget(splitter)
        self.setCentralWidget(root)

        self.search_input.textChanged.connect(self.on_query_changed)
        self.command_view.selectionModel().currentChanged.connect(self.update_preview)

        self.select_first_row()

    @beartype
    def on_query_changed(self, query: str) -> None:
        self.proxy_model.set_query(query)
        self.select_first_row()

    @beartype
    def select_first_row(self) -> None:
        if self.proxy_model.rowCount() == 0:
            self.preview.setPlainText("")
            return
        first_index = self.proxy_model.index(0, 0)
        self.command_view.setCurrentIndex(first_index)

    def update_preview(self, current: QModelIndex, previous: QModelIndex) -> None:
        if not current.isValid():
            self.preview.setPlainText("")
            return
        command = self.proxy_model.data(current, CommandListModel.command_role)
        if not isinstance(command, str):
            raise ValueError(f"Expected command string for selected row, got {command!r}")
        self.preview.setPlainText(command)

    @beartype
    def copy_selected_command(self, checked: bool = False) -> None:
        current = self.command_view.currentIndex()
        if not current.isValid():
            raise ValueError("Cannot copy command because no command is currently selected")
        command = self.proxy_model.data(current, CommandListModel.command_role)
        if not isinstance(command, str):
            raise ValueError(f"Expected command string for selected row, got {command!r}")
        QApplication.clipboard().setText(command)
        self.close()



@beartype
def load_jsonl_commands(path: Path) -> List[CommandEntry]:
    if not path.exists():
        raise FileNotFoundError(f"Input file does not exist: {path}")
    if not path.is_file():
        raise ValueError(f"Input path is not a file: {path}")

    entries: List[CommandEntry] = []
    for line_number, line in enumerate(path.read_text(encoding="utf-8").splitlines(), start=1):
        if line.strip() == "":
            continue
        raw_value = json.loads(line)
        match raw_value:
            case {"command": str() as command, "count": int() as count}:
                entries.append(CommandEntry(command=command, count=count, order=line_number))
            case _:
                raise ValueError(
                    f"Invalid JSON object at line {line_number}: expected keys "
                    f"'command' (str) and 'count' (int), got {raw_value!r}"
                )

    if len(entries) == 0:
        raise ValueError(f"Input file has no valid command entries: {path}")

    return entries


@beartype
def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("jsonl_path", type=Path)
    return parser.parse_args()


@beartype
def main() -> int:
    args = parse_args()
    entries = load_jsonl_commands(args.jsonl_path)

    app = QApplication(sys.argv)
    window = CommandSelectorWindow(entries, args.jsonl_path)
    window.show()
    return app.exec()


if __name__ == "__main__":
    raise SystemExit(main())
