#!/usr/bin/env python

from typing import List
import subprocess
from pathlib import Path


def get_connected_screens() -> List[str]:
    result = subprocess.run(["xrandr"], capture_output=True, text=True)
    screens = []
    for line in result.stdout.split("\n"):
        if " connected" in line:
            parts = line.replace("primary", "").split()
            if len(parts) >= 3:
                screens.append(parts[2])
    return screens


def get_current_screen_index() -> int:
    state_file = Path.home() / ".wacom_screen_state"
    if state_file.exists():
        return int(state_file.read_text().strip())
    return 0


def save_current_screen_index(index: int) -> None:
    state_file = Path.home() / ".wacom_screen_state"
    state_file.write_text(str(index))


def get_wacom_devices() -> List[str]:
    result = subprocess.run(["xsetwacom", "list", "devices"],
                            capture_output=True,
                            text=True)
    devices = []
    for line in result.stdout.split("\n"):
        if "id:" in line:
            device_id = line.split("id:")[1].strip().split()[0]
            devices.append(device_id)
    return devices


def configure_wacom_devices(screen: str, rotation: str) -> None:
    devices = get_wacom_devices()
    for device in devices:
        subprocess.run(["xsetwacom", "set", device, "MapToOutput", screen])
        subprocess.run(["xsetwacom", "set", device, "Rotate", rotation])


screens = get_connected_screens()
current_index = get_current_screen_index()
next_index = (current_index + 1) % len(screens)
selected_screen = screens[next_index]
save_current_screen_index(next_index)

configure_wacom_devices(selected_screen, "none")
