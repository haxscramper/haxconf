#!/usr/bin/env python
#
import sys
import subprocess
import itertools
import readchar
import rich_click as click
import select
import termios
import tty
from pathlib import Path
from threading import Thread
import os
from typing import *
from pprint import pprint

from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler
from PyQt5.QtCore import (
    QObject,
    pyqtSignal,
    QCoreApplication,
    QThread,
    pyqtSlot,
)

from pathspec import PathSpec
from pathspec.patterns import GitWildMatchPattern

# Replace this with your desired directories to watch
directories_to_watch = [os.getcwd()]

import logging
from rich.logging import RichHandler

logging.basicConfig(
    level="NOTSET",
    format="%(message)s",
    datefmt="[%X]",
    handlers=[
        RichHandler(
            rich_tracebacks=True,
            markup=True,
            enable_link_path=False,
            show_time=False,
        )
    ],
)

for name in logging.root.manager.loggerDict:
    logger = logging.getLogger(name)
    logger.setLevel(logging.WARNING)

    log = logging.getLogger("rich")
    log.setLevel(logging.DEBUG)

    log = logging.getLogger("rich")
    log.setLevel(logging.DEBUG)

log = logging.getLogger("rich")


class ShellCmd:
    def __init__(
        self,
        cmd: List[str],
        stdin: Optional[str] = None,
        joinOut: bool = False,
        mightBeMissing: bool = False,
    ):
        self.cmd = cmd
        self.stdin = stdin
        self.joinOut = joinOut
        self.mightBeMissing = mightBeMissing


class CommandExecutor(QObject):
    started = pyqtSignal()
    finished = pyqtSignal()
    error_occurred = pyqtSignal(str)
    exit_app = pyqtSignal()
    execute = pyqtSignal()

    def __init__(self, commands: List[List[ShellCmd]]):
        super().__init__()
        self.commands = commands
        self.current_process = None

    def execute_chain(self, chain: List[ShellCmd]) -> bool:
        processes = []
        log_handle = sys.stdout

        for i, command in enumerate(chain):
            if command.mightBeMissing and not os.path.exists(
                command.cmd[0]
            ):
                log.error(f"{command.cmd} is missing, skipping execution")
                return False

            log.debug(command.cmd)
            process = subprocess.Popen(
                command.cmd,
                stdin=(
                    command.stdin if i == 0 else processes[i - 1].stdout
                ),
                stdout=(
                    subprocess.PIPE if i < len(chain) - 1 else log_handle
                ),
                stderr=(subprocess.STDOUT),
                # stderr=(
                #     subprocess.PIPE
                #     if command.joinOut
                #     else subprocess.STDOUT
                # ),
            )
            if i > 0:
                # Allow the previous process to receive a SIGPIPE if
                # the current process exits.
                processes[i - 1].stdout.close()

            processes.append(process)

            if i == 0:
                self.current_process = process

        return True

    @pyqtSlot()
    def execute_commands(self):
        self.started.emit()
        for command in self.commands:
            if not self.execute_chain(command):
                log.warning(
                    "Chain command failed to execute, stopping the full list"
                )
                break

            try:
                return_code = self.current_process.wait()
            except KeyboardInterrupt:
                self.current_process.terminate()
                self.error_occurred.emit(f"Interrupted command: {command}")
                break

            if return_code != 0:
                log.error(f"Failed command: {command}")
                break
        self.finished.emit()

    @pyqtSlot()
    def kill_current_command(self):
        if self.current_process and self.current_process.poll() is None:
            self.current_process.terminate()
            self.current_process.wait()
            self.error_occurred.emit(
                f"Killed command: {' '.join(self.current_process.args)}"
            )

    @pyqtSlot()
    def restart_commands(self):
        log.warning("Requested command restart")
        self.kill_current_command()
        self.execute_commands()


class CustomFileSystemEventHandler(FileSystemEventHandler):
    def __init__(self, executor):
        super().__init__()
        self.executor = executor

    def on_any_event(self, event):
        if event.event_type in ["modified", "created", "deleted"]:
            log.info(f"Directory event {event}")
            log.info(f"{type(event.event_type)}")
            self.executor.execute.emit()


class DirectoryWatcher(QObject):
    file_modified = pyqtSignal()
    file_created = pyqtSignal()
    file_deleted = pyqtSignal()

    directory_modified = pyqtSignal()
    directory_created = pyqtSignal()
    directory_deleted = pyqtSignal()

    def __init__(self, directories):
        super().__init__()
        self.directories = directories
        self.event_handler = FileSystemEventHandler()
        self.event_handler.on_modified = self.on_modified
        self.observer = Observer()

        # Load ignore rules from .gitignore and .fdignore files
        self.ignore_patterns = self.load_ignore_patterns()

    def load_ignore_patterns(self):
        current_dir = os.path.abspath(os.getcwd())
        pattern_groups = []

        while True:
            for file_name in [".gitignore", ".fdignore"]:
                file_path = os.path.join(current_dir, file_name)
                try:
                    with open(file_path, "r") as f:
                        log.info(f"Using ignore patterns from {file_path}")
                        pattern_groups.append(f.read())

                except FileNotFoundError:
                    pass

            parent_dir = os.path.dirname(current_dir)
            if parent_dir == current_dir:
                break
            current_dir = parent_dir

        return PathSpec.from_lines(
            GitWildMatchPattern,
            list(
                itertools.chain.from_iterable(
                    [it.splitlines() for it in reversed(pattern_groups)]
                )
            ),
        )

    def start(self):
        for directory in self.directories:
            self.observer.schedule(
                self.event_handler, directory, recursive=True
            )
        self.observer.start()

    def stop(self):
        self.observer.stop()
        self.observer.join()

    def on_modified(self, event):
        if not self.ignore_patterns.match_file(event.src_path):
            match event.event_type:
                case "modified":
                    if not event.is_directory:
                        log.info(event.src_path)
                        self.file_modified.emit()

                case "deleted":
                    if not event.is_directory:
                        log.info(event.src_path)
                        self.file_deleted.emit()

                case "created":
                    if not event.is_directory:
                        log.info(event.src_path)
                        self.file_created.emit()


def set_raw_mode(file_descriptor):
    original_settings = termios.tcgetattr(file_descriptor)
    tty.setraw(file_descriptor)
    return original_settings


def restore_mode(file_descriptor, original_settings):
    termios.tcsetattr(
        file_descriptor, termios.TCSADRAIN, original_settings
    )


def watch_space_key(executor):
    file_descriptor = sys.stdin.fileno()
    # original_settings = set_raw_mode(file_descriptor)

    try:
        while True:
            key = readchar.readkey()
            if key == " ":
                executor.execute.emit()

            elif key == "q":
                raise KeyboardInterrupt("")

            else:
                log.info(f"Key: {key}, {ord(key)}")

    except KeyboardInterrupt:
        log.debug("Interrup acceped, exiting")
        executor.exit_app.emit()  # Emit exit_app signal

    finally:
        pass
        # restore_mode(file_descriptor, original_settings)


def start_main_loop(commands: List[ShellCmd], directories: List[str]):
    log.info("Starting listener")
    app = QCoreApplication([sys.argv[0]])
    # Replace this with the shell commands you want to execute
    executor = CommandExecutor(commands)

    executor.execute.connect(executor.restart_commands)

    # Watch directories for filesystem events
    watcher_thread = QThread()
    directory_watcher = DirectoryWatcher(directories)
    directory_watcher.file_modified.connect(executor.execute)
    directory_watcher.moveToThread(watcher_thread)
    watcher_thread.started.connect(directory_watcher.start)

    watcher_thread.start()

    def on_exit():
        watcher_thread.quit()
        watcher_thread.wait()
        app.quit()

    executor.exit_app.connect(on_exit)

    # Watch for space key press in the terminal
    space_key_watcher = Thread(
        target=watch_space_key, args=(executor,), daemon=True
    )
    space_key_watcher.start()

    sys.exit(app.exec_())


def create_missing_file(file_path: str, content: str):
    if os.path.exists(file_path):
        log.debug(f"File '{file_path}' already exists.")
    else:
        try:
            with open(file_path, "w") as file:
                file.write(content)

            log.debug(f"File '{file_path}' created.")

        except Exception as e:
            log.error(f"Error creating file '{file_path}': {e}")


@click.group()
def cli():
    pass


@cli.command("dot")
@click.argument("file", type=click.Path())
def exec_dot(file):
    file = os.path.abspath(file)
    create_missing_file(
        file,
        """
digraph G {
  node[shape=rect];
  A -> B;
}
    """,
    )

    format = "png"
    result = os.path.abspath(file.replace(".dot", "." + format))

    head = ["dot", "-T" + format]

    CommandExecutor(
        [[ShellCmd(head + ["-o" + result, file])]]
    ).execute_commands()

    subprocess.Popen(["sxiv", result])

    start_main_loop(
        [
            [ShellCmd(head + ["-o" + result + ".tmp", file])],
            [ShellCmd(["cp", result + ".tmp", result])],
        ],
        [os.getcwd()],
    )


@cli.command("cpp")
@click.argument("file", type=click.Path())
@click.option("--options", type=click.Path(), help="?")
@click.option(
    "--wrapper", type=click.Choice(["lldb", "rr", "valgrind", "firejail"])
)
@click.option("--opt", multiple=True, default=[])
@click.option("--run-input", type=click.Path())
@click.option(
    "--macro-expand", type=click.BOOL, is_flag=True, default=False
)
@click.option(
    "--no-std-include", type=click.BOOL, is_flag=True, default=False
)
@click.option("--binary", "binary_override", type=click.Path())
def exec_cpp(
    file,
    options,
    wrapper,
    opt,
    run_input,
    macro_expand,
    no_std_include,
    binary_override,
):
    file = os.path.abspath(file)
    create_missing_file(
        file,
        """
#include <iostream>

int main() {
    std::cout << "1\\n";
    return 0;
}
    """,
    )

    scripts: str = os.path.expanduser("~/.config/haxconf/scripts")
    binary: str = (
        binary_override
        if binary_override
        else os.path.abspath(file.replace(".cpp", ".bin"))
    )

    run_cmds: List[str] = []
    compile_cmds: List[ShellCmd] = []

    base_name = os.path.splitext(file)[0]  # Remove the original extension
    cfg_file = base_name + ".cfg"  # Append .cfg to the base name

    config_cmds = []
    if os.path.exists(cfg_file):
        log.info(f"Found configuration file {cfg_file}")
        config_cmds.append("@" + cfg_file)

    if macro_expand:
        cmds = [
            "clang++",
            "-E",
            "-P",
            "-std=c++20",
            "-fno-color-diagnostics",
        ]

        if no_std_include:
            cmds.append("-nostdinc++")

        cmds.append(file)

        compile_cmds = [ShellCmd(cmds)]

    else:
        if file.endswith("CMakeLists.txt"):
            compile_cmds = [
                ShellCmd(
                    [
                        "cmake",
                        "-B",
                        "build",
                        "--log-level",
                        "TRACE",
                        "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON",
                        ".",
                    ]
                ),
                ShellCmd(["ninja", "-C", "build"]),
            ]

        else:
            compile_cmds = [
                ShellCmd(
                    [
                        "clang++",
                        "-ferror-limit=1",
                        "-std=c++20",
                        "-g",
                        "-fdiagnostics-color=always",
                    ]
                    + config_cmds
                    + [
                        "-o",
                        binary,
                        file,
                    ],
                    joinOut=True,
                )
            ]

    match wrapper:
        case "lldb":
            run_cmds = [
                "lldb",
                "--batch",
                "-o",
                f"command script import {scripts}/lldb_script.py",
                "-o",
                "run",
                "--source-on-crash",
                f"{scripts}/lldb_script.txt",
                "--",
                binary,
            ] + [o for o in opt]

        case _:
            run_cmds = [binary] + [o for o in opt]

    filters = [
        r"is a Catch2",
        r"Run with -\? for options",
        r"Catch will terminate",
        r"\(lldb\)",
        r"frame-format",
        r"  0x",
        r"libc\.so",
        r"Process \d{2,}",
        r"Executing commands in",
        r"Terminate called without",
        r"\.{10,}",
        r"-{10,}",
        r"~{10,}",
        r"Test failure requires aborting",
        r"terminate called without",
    ]

    rg_filter = ["rg", "-v", "(" + "|".join(filters) + ")"]

    if macro_expand:
        start_main_loop(
            [
                compile_cmds
                + [
                    ShellCmd(
                        ["clang-format", "--assume-filename=test.hpp"]
                    ),
                    ShellCmd(["bat", "--language=cpp", "--no-pager"]),
                ]
            ],
            [os.getcwd()],
        )

    else:
        start_main_loop(
            [
                compile_cmds
                + [
                    ShellCmd(["tee", "/tmp/cpp-rebuild-errors"]),
                ],
                [
                    ShellCmd(run_cmds, joinOut=True, mightBeMissing=True),
                    ShellCmd(rg_filter),
                ],
            ],
            [os.getcwd()],
        )


def main():
    cli()


if __name__ == "__main__":
    main()
