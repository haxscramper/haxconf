#!/usr/bin/env python

import click
from plumbum import local


@click.group()
def cli():
    pass


@cli.command()
@click.argument("window_name")
def focus_on_window(window_name):
    xprop = local["xprop"]
    wmctrl = local["wmctrl"]

    current_desktop = int(xprop("-root", "_NET_CURRENT_DESKTOP").split()[-1])
    base_tag = (current_desktop // 9) * 9

    windows = [it for it in wmctrl("-l").splitlines() if it.startswith("0x")]
    print(f"Current desktop: {current_desktop}")
    for w in windows:
        print(w)

    def get_window_for_name(window_list):
        return next(
            (line.split()[0]
             for line in window_list if window_name.lower() in line.lower()),
            None)

    windows_on_current_tag = [
        line for line in windows
        if base_tag <= int(line.split()[1]) < base_tag + 9
    ]

    desired_window = get_window_for_name(windows_on_current_tag)

    if desired_window:
        print(desired_window)
        wmctrl("-i", "-a", desired_window)

    else:
        print(f"No specific window found, fallback to wmctrl first window")
        first_window = get_window_for_name(windows)
        if first_window:
            wmctrl("-i", "-a", first_window)


if __name__ == "__main__":
    cli()
