#!/usr/bin/env python

from typing import *
from subprocess import *
import os

res: str = run(
    ["xrandr", "--listmonitors"], stdout = PIPE).stdout.decode("utf-8")

args: List[str] = ["feh"]
dir: str = os.path.expanduser("~/defaultdirs/images/wallpapers")
count: int = 0
for line in res.splitlines():
    split = line.split(" ")
    if 4 < len(split):
        hasFile: bool = False
        name: str = f"{dir}/bg-{count}"
        if os.path.isfile(name):
            count += 1
            args.append("--bg-max")
            args.append(name)
            print(f"assignmed {name} to {split[-1]}")
            hasFile = True

        else:
            print(
                f"No file found for wallpaper on screen {count + 1} -",
                f"file {name} does not exist."
            )
            quit(1)

print(args)
run(args)
