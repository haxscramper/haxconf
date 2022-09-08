#!/usr/bin/env python

#!/usr/bin/env python

from typing import *
from subprocess import *
import random
import os
import time

res: str = run(["xrandr", "--listmonitors"], stdout=PIPE).stdout.decode(
    "utf-8"
)
images = []

sub_args = [
    "fd",
    ".",
    os.path.expanduser("~/defaultdirs/images"),
    "-t",
    "file",
]

sub = run(
    sub_args,
    stdout=PIPE,
)

std = sub.stdout.decode("utf-8")

for image in std.split("\n"):
    images.append(image)

while True:
    args: List[str] = ["feh"]
    for image in random.sample(images, 4):
        args.append("--bg-max")
        args.append("--scale-down")
        args.append(image)

    print(args)
    run(args)
    time.sleep(60)
