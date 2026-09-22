#!/usr/bin/env python

import os
import re
import subprocess
import sys
import uuid

MONITOR_PATTERN = re.compile(r"(\d+)/\d+x(\d+)/\d+\+(-?\d+)\+(-?\d+)")


def get_pointer_position():
    output = subprocess.check_output(
        ["xdotool", "getmouselocation", "--shell"],
        text=True,
    )

    values = dict(line.split("=", 1) for line in output.splitlines())
    return int(values["X"]), int(values["Y"])


def get_current_monitor(pointer_x, pointer_y):
    output = subprocess.check_output(
        ["xrandr", "--listactivemonitors"],
        text=True,
    )

    for line in output.splitlines():
        match = MONITOR_PATTERN.search(line)
        if not match:
            continue

        width, height, x, y = map(int, match.groups())

        if x <= pointer_x < x + width and y <= pointer_y < y + height:
            return x, y, width, height

    raise RuntimeError("The mouse pointer is not on an active monitor")


def main():
    pointer_x, pointer_y = get_pointer_position()
    monitor_x, monitor_y, monitor_width, monitor_height = (get_current_monitor(
        pointer_x, pointer_y))

    width = monitor_width * 60 // 100
    height = monitor_height * 80 // 100
    x = monitor_x + (monitor_width - width) // 2
    y = monitor_y + (monitor_height - height) // 2

    window_class = f"centered-kitty-{os.getpid()}-{uuid.uuid4().hex}"

    subprocess.Popen(["kitty", "--class", window_class, *sys.argv[1:]], )

    window_id = subprocess.check_output(
        [
            "xdotool",
            "search",
            "--sync",
            "--onlyvisible",
            "--class",
            f"^{window_class}$",
        ],
        text=True,
    ).splitlines()[0]

    subprocess.run(
        [
            "wmctrl",
            "-ir",
            hex(int(window_id)),
            "-e",
            f"0,{x},{y},{width},{height}",
        ],
        check=True,
    )


if __name__ == "__main__":
    main()
