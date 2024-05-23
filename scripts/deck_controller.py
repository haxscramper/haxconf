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

    windows = wmctrl("-l").splitlines()
    print(f"Current desktop: {current_desktop}")
    for w in windows:
        print(w)

    windows_on_current_tag = [
        line for line in windows
        if base_tag <= int(line.split()[1]) < base_tag + 9
    ]
    desired_window = next((line.split()[0] for line in windows_on_current_tag
                           if window_name.lower() in line.lower()), None)

    if desired_window:
        print(desired_window)
        wmctrl("-i", "-a", desired_window)


if __name__ == "__main__":
    cli()
