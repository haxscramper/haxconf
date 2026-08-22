#!/usr/bin/env python

from typing import List
import subprocess
from pathlib import Path
import tkinter as tk


def get_connected_screens() -> List[str]:
    result = subprocess.run(["xrandr"],
                            capture_output=True,
                            text=True,
                            check=True)
    screens = []
    for line in result.stdout.splitlines():
        if " connected" in line:
            parts = line.split()
            if parts:
                screens.append(parts[0])
    return screens


def get_output_geometry(output: str) -> tuple[int, int, int, int]:
    result = subprocess.run(["xrandr"],
                            capture_output=True,
                            text=True,
                            check=True)
    for line in result.stdout.splitlines():
        if not line.startswith(output + " "):
            continue
        parts = line.split()
        for part in parts:
            if "x" in part and "+" in part:
                size_part, x_pos, y_pos = part.split("+")
                width, height = size_part.split("x")
                return int(width), int(height), int(x_pos), int(y_pos)
    raise RuntimeError(f"Could not determine geometry for output {output}")


def get_current_screen_index() -> int:
    state_file = Path.home() / ".wacom_screen_state"
    if state_file.exists():
        return int(state_file.read_text().strip())
    return 0


def save_current_screen_index(index: int) -> None:
    state_file = Path.home() / ".wacom_screen_state"
    state_file.write_text(str(index))


def get_wacom_devices() -> List[str]:
    result = subprocess.run(
        ["xsetwacom", "list", "devices"],
        capture_output=True,
        text=True,
        check=True,
    )
    devices = []
    for line in result.stdout.splitlines():
        if "id:" in line:
            device_id = line.split("id:")[1].strip().split()[0]
            devices.append(device_id)
    return devices


def configure_wacom_devices(screen: str, rotation: str) -> None:
    devices = get_wacom_devices()
    for device in devices:
        subprocess.run(
            ["xsetwacom", "set", device, "MapToOutput", screen],
            check=True,
        )
        subprocess.run(
            ["xsetwacom", "set", device, "Rotate", rotation],
            check=True,
        )


def move_cursor_to_screen_center(x: int, y: int, width: int,
                                 height: int) -> None:
    center_x = x + width // 2
    center_y = y + height // 2
    subprocess.run(
        ["xdotool", "mousemove",
         str(center_x), str(center_y)], check=True)


def flash_overlay(x: int, y: int, width: int, height: int,
                  screen_name: str) -> None:
    overlay_width = width // 2
    overlay_height = height // 2
    overlay_x = x + (width - overlay_width) // 2
    overlay_y = y + (height - overlay_height) // 2

    root = tk.Tk()
    root.overrideredirect(True)
    root.attributes("-topmost", True)
    root.attributes("-alpha", 0.35)
    root.configure(bg="red")
    root.geometry(f"{overlay_width}x{overlay_height}+{overlay_x}+{overlay_y}")

    label = tk.Label(
        root,
        text=screen_name,
        font=("Sans", 48, "bold"),
        fg="white",
        bg="red",
    )
    label.place(relx=0.5, rely=0.5, anchor="center")

    root.after(500, root.destroy)
    root.mainloop()


screens = get_connected_screens()
current_index = get_current_screen_index()
next_index = (current_index + 1) % len(screens)
selected_screen = screens[next_index]
save_current_screen_index(next_index)

configure_wacom_devices(selected_screen, "none")

width, height, x, y = get_output_geometry(selected_screen)
move_cursor_to_screen_center(x, y, width, height)
flash_overlay(x, y, width, height, selected_screen)
