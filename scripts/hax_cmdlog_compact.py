#!/usr/bin/env python
import json
import sys
from collections import OrderedDict
from pathlib import Path


def main() -> None:
    path = Path(sys.argv[1])
    counts = OrderedDict()

    with path.open() as infile:
        for line in infile:
            line = line.strip()
            if not line:
                continue
            obj = json.loads(line)
            cmd = obj["command"]
            counts[cmd] = counts.get(cmd, 0) + int(obj.get("count", 1))

    tmp = path.with_suffix(path.suffix + ".tmp")
    with tmp.open("w") as out:
        for command, count in counts.items():
            json.dump({"command": command, "count": count}, out, separators=(",", ":"))
            out.write("\n")

    tmp.replace(path)


if __name__ == "__main__":
    main()
