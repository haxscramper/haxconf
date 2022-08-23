#!/usr/bin/env python

#!/usr/bin/env python

from typing import *
from subprocess import *
import random
import os
import time

res: str = run(["xrandr", "--listmonitors"], stdout=PIPE).stdout.decode("utf-8")
dir: str = os.path.expanduser("~/defaultdirs/images")
print(res)
images = []
for root, subdirs, files in os.walk(dir):
    for file in files:
        images.append(os.path.join(root, file))

while True:
    args: List[str] = ["feh"]
    for image in random.sample(images, 4):
        args.append("--bg-center")
        args.append("--scale-down")
        args.append(image)

    print(args)
    run(args)
    time.sleep(15)
