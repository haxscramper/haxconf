#!/usr/bin/env python

import sys
import logging
from dataclasses import dataclass
from typing import List, Dict
from plumbum import local
from pathlib import Path

logging.basicConfig(
    level=logging.DEBUG,
    format="%(asctime)s - %(filename)s:%(lineno)d - %(levelname)s - %(message)s"
)
logger = logging.getLogger(__name__)
logging.getLogger("plumbum.local").setLevel(logging.WARNING)




@dataclass
class WindowInfo:
    window_id: str
    screen: int
    x: int
    y: int
    width: int
    height: int
    window_class: str
    tags: List[str]
    is_visible: bool


def get_awesome_clients() -> List[WindowInfo]:
    import csv
    from io import StringIO

    awesome_client = local["awesome-client"]
    script_path = Path(__file__).parent / "get_window_list.lua"

    result = awesome_client < str(script_path)
    cmd_result = result.run()
    logger.debug(f"Raw awesome-client output: {result}")

    output = cmd_result[1].strip()

    if output.startswith('   string "'):
        output = output[11:]
    if output.endswith('"'):
        output = output[:-1]

    windows = []
    csv_reader = csv.reader(StringIO(output))

    for row in csv_reader:
        if len(row) >= 9:
            logger.debug(f"Parsed window: {row}")
            tags = row[7].split(";") if row[7] else []
            windows.append(
                WindowInfo(window_id=row[0],
                           screen=int(row[1]),
                           x=int(row[2]),
                           y=int(row[3]),
                           width=int(row[4]),
                           height=int(row[5]),
                           window_class=row[6],
                           tags=tags,
                           is_visible=row[8] == "true"))

    logger.debug(f"Found {len(windows)} windows")
    return windows


def focus_emacs_split(screen: int, position: str) -> bool:
    windows = get_awesome_clients()
    for w in windows:
        if w.window_class == "Emacs":
            logger.info(f"{w}, {w.screen == screen}, {w.is_visible}")

    emacs_windows = [
        w for w in windows
        if w.window_class == "Emacs" and w.screen == screen and w.is_visible
    ]

    logger.debug(f"Looking for visible Emacs windows on screen {screen}")
    logger.debug(f"Found emacs windows: {emacs_windows}")

    if not emacs_windows:
        logger.error(f"No visible emacs window found on screen {screen}")
        return False

    emacs_window = emacs_windows[0]
    logger.debug(f"Using emacs window: {emacs_window}")

    awesome_client = local["awesome-client"]
    focus_cmd = f"""
    local awful = require('awful')
    for _, c in ipairs(client.get()) do
        if tostring(c.window) == "{emacs_window.window_id}" then
            client.focus = c
            c:raise()
            break
        end
    end
    """

    awesome_client(focus_cmd)
    logger.debug(f"Focused window {emacs_window.window_id}")

    splits = get_emacs_splits()

    if len(splits) < 2:
        logger.info(f"Emacs window on screen {screen} has no splits")
        return True

    if position == "left":
        target_split = min(splits, key=lambda s: s["left"])
    elif position == "right":
        target_split = max(splits, key=lambda s: s["left"])
    else:
        logger.error(f"Invalid position: {position}")
        return False

    logger.debug(f"Target split: {target_split}")

    emacsclient = local["emacsclient"]
    elisp_code = f"""
    (let ((target-window nil))
      (walk-windows
       (lambda (win)
         (let ((edges (window-edges win)))
           (when (and (= (nth 0 edges) {target_split["left"]})
                      (= (nth 1 edges) {target_split["top"]})
                      (= (nth 2 edges) {target_split["right"]})
                      (= (nth 3 edges) {target_split["bottom"]}))
             (setq target-window win))))
       nil t)
      (when target-window
        (select-window target-window)))
    """

    emacsclient("-e", elisp_code)
    logger.debug("Selected emacs window split")
    return True


def get_emacs_splits() -> List[Dict[str, int]]:
    import csv
    from io import StringIO
    
    emacsclient = local["emacsclient"]

    elisp_code = """
    (let ((splits '())
          (result ""))
      (walk-windows
       (lambda (win)
         (let ((edges (window-edges win)))
           (setq result (concat result (format "%d,%d,%d,%d\\n" (nth 0 edges) (nth 1 edges) (nth 2 edges) (nth 3 edges))))))
       nil (selected-frame))
      result)
    """

    result = emacsclient("-e", elisp_code)
    logger.debug(f"Emacs splits output: {result}")

    clean_result = result.strip().strip('"').replace('\\n', '\n')
    
    splits = []
    csv_reader = csv.reader(StringIO(clean_result))
    
    for row in csv_reader:
        if len(row) >= 4:
            splits.append({
                "left": int(row[0]),
                "top": int(row[1]),
                "right": int(row[2]),
                "bottom": int(row[3])
            })

    logger.debug(f"Found {len(splits)} emacs splits")
    for s in splits:
        logger.info(s)
    return splits



def get_screen_layout() -> Dict[str, int]:
    awesome_client = local["awesome-client"]

    layout_cmd = """
    local screens = {}
    for s in screen do
        table.insert(screens, string.format('"%d","%d","%d"',
            s.index, s.geometry.x, s.geometry.y))
    end
    return table.concat(screens, '\\n')
    """

    result = awesome_client(layout_cmd)
    logger.debug(f"Screen layout output: {result}")

    output = result.strip()
    if output.startswith('string "'):
        output = output[8:]
    if output.endswith('"'):
        output = output[:-1]

    logger.info(list(output))
    screens = []
    import csv
    from io import StringIO
    csv_reader = csv.reader(StringIO(output))
    for row in csv_reader:
        logger.info(row)
        screens.append({
            'index': int(row[0]),
            'x': int(row[1]),
            'y': int(row[2])
        })

    logger.debug(f"Found screens: {screens}")

    screens.sort(key=lambda s: (s['x'], s['y']))

    left_screens = [
        s for s in screens if s['x'] < max(screens, key=lambda x: x['x'])['x']
    ]
    right_screens = [
        s for s in screens if s['x'] >= max(screens, key=lambda x: x['x'])['x']
    ]

    left_screens.sort(key=lambda s: s['y'])

    layout_map = {}

    if len(left_screens) >= 1:
        layout_map['left-upper'] = left_screens[0]['index']
    if len(left_screens) >= 2:
        layout_map['left-lower'] = left_screens[1]['index']
    if len(right_screens) >= 1:
        layout_map['right'] = right_screens[0]['index']

    logger.debug(f"Screen layout map: {layout_map}")
    return layout_map


def visual_position_to_screen(position: str) -> int:
    layout = get_screen_layout()
    if position not in layout:
        available = list(layout.keys())
        raise ValueError(
            f"Invalid screen position '{position}'. Available: {available}")
    return layout[position]


def main() -> None:
    if len(sys.argv) != 3:
        logger.error("Usage: script.py <screen_position> <window_position>")
        logger.error("Screen positions: left-upper, left-lower, right")
        logger.error("Window positions: left, right")
        sys.exit(1)

    screen_position = sys.argv[1].lower()
    window_position = sys.argv[2].lower()

    if window_position not in ["left", "right"]:
        logger.error("Window position must be 'left' or 'right'")
        sys.exit(1)

    screen = visual_position_to_screen(screen_position)

    success = focus_emacs_split(screen, window_position)
    if not success:
        sys.exit(1)


if __name__ == "__main__":
    main()
