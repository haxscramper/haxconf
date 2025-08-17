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
    window_id: int
    screen: int
    x: int
    y: int
    width: int
    height: int
    window_class: str
    tags: List[str]
    is_visible: bool


def trim_awesome_output(raw_output: str) -> str:
    output = raw_output.strip()
    
    if output.startswith('string "'):
        output = output[8:]
    if output.endswith('"'):
        output = output[:-1]
    
    return output

def get_awesome_clients() -> List[WindowInfo]:
    import csv
    from io import StringIO

    awesome_client = local["awesome-client"]
    script_path = Path(__file__).parent / "get_window_list.lua"

    result = awesome_client < str(script_path)
    cmd_result = result.run()
    # logger.debug(f"Raw awesome-client output: {result}")

    output = trim_awesome_output(cmd_result[1])
    windows = []
    csv_reader = csv.reader(StringIO(output))

    for row in csv_reader:
        if len(row) >= 9:
            # logger.debug(f"Parsed window: {row}")
            tags = row[7].split(";") if row[7] else []
            windows.append(
                WindowInfo(window_id=int(row[0]),
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
    # logger.debug(f"Found emacs windows: {emacs_windows}")

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

    splits = get_emacs_splits(emacs_window)

    if len(splits) < 2:
        logger.info(f"Emacs window on screen {screen} has no splits")
        return True

    if position == "left":
        target_split = min(splits, key=lambda s: s.left)
    elif position == "right":
        target_split = max(splits, key=lambda s: s.left)
    else:
        logger.error(f"Invalid position: {position}")
        return False

    logger.debug(f"Target split: {target_split}")

    emacsclient = local["emacsclient"]
    elisp_code = f"(hax/select-window-split-by-position \"{target_split.frame_id}\" {target_split.left} {target_split.top} {target_split.right} {target_split.bottom})"  
    emacsclient("-e", elisp_code)
    logger.debug("Selected emacs window split")
    return True

@dataclass
class EmacsSplitInformation():
    left: int
    top: int
    right: int 
    bottom: int
    frame_id: int

def get_emacs_splits(emacs_window: WindowInfo) -> List[EmacsSplitInformation]:
    import csv
    from io import StringIO
    
    emacsclient = local["emacsclient"]

    elisp_code = "(hax/list-window-splits)"

    result = emacsclient("-e", elisp_code)
    logger.debug(f"Emacs splits output: {result}")

    clean_result = result.strip().strip('"').replace('\\n', '\n')
    
    splits = []
    csv_reader = csv.reader(StringIO(clean_result))
    
    for row in csv_reader:
        if len(row) >= 4:
            info = EmacsSplitInformation(
                frame_id=int(row[0]),
                left=int(row[1]),
                top=int(row[2]),
                right=int(row[3]),
                bottom=int(row[4])
            )

            if info.frame_id == emacs_window.window_id:
                splits.append(info)

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

    output = trim_awesome_output(awesome_client(layout_cmd))
    logger.debug(f"Screen layout output: {output}")

    screens = []
    import csv
    from io import StringIO
    csv_reader = csv.reader(StringIO(output))
    for row in csv_reader:
        # logger.info(row)
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
