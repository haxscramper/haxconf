#!/usr/bin/env python

import re
import subprocess
import sys
from pathlib import Path

relativeFile, file, workspaceFolder, lineNumber, fileExtname, selectedText = sys.argv[1:]

language = fileExtname.removeprefix(".")

if language == "hpp":
    language = "cpp"

elif file.endswith("CMakeLists.txt"):
    language = "cmake"

if str(file).startswith(str(workspaceFolder)):
    file = relativeFile


selectedText = selectedText.strip("\n")

markdown = f"""
`{file}:{lineNumber}`:

```{language}
{selectedText}
```
"""

print(markdown)

subprocess.run(["copyq", "add", "-"], input=markdown, text=True, check=True)
subprocess.run(["copyq", "copy", "-"], input=markdown, text=True, check=True)
subprocess.run(["notify-send", "Selection OK"], text=True, check=True)
