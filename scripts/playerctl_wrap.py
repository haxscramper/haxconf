#!/usr/bin/env python
import subprocess
import sys
import logging
from typing import Optional


def get_playing_players() -> list[str]:
    """Get list of player names that are currently playing"""
    result = subprocess.run(["playerctl", "--all-players", "status"],
                            capture_output=True,
                            text=True,
                            check=True)
    status_lines = result.stdout.strip().split('\n')

    # Get player names
    result = subprocess.run(["playerctl", "--list-all"],
                            capture_output=True,
                            text=True,
                            check=True)
    player_names = result.stdout.strip().split('\n')

    playing_players = []
    for i, status in enumerate(status_lines):
        if status.strip() == "Playing" and i < len(player_names):
            playing_players.append(player_names[i])

    return playing_players


def get_current_position(player: Optional[str] = None) -> int:
    cmd = ["playerctl", "position"]
    if player:
        cmd = ["playerctl", "--player", player, "position"]

    result = subprocess.run(cmd, capture_output=True, text=True, check=True)
    print(f"Current position for {player or 'default'}: {result.stdout}")
    return int(float(result.stdout.strip()))


def set_position(position: int, player: Optional[str] = None) -> None:
    cmd = ["playerctl", "position", "--", str(position)]
    if player:
        cmd = [
            "playerctl", "--player", player, "position", "--",
            str(position)
        ]

    print(f"Setting position {position} for {player or 'default'}")
    subprocess.run(cmd, check=True)


def rel_position(offset: int) -> None:
    playing_players = get_playing_players()

    if not playing_players:
        logging.error("No players are currently playing")
        sys.exit(1)

    for player in playing_players:
        try:
            current_pos = get_current_position(player)
            new_pos = current_pos + offset
            set_position(new_pos, player)
        except subprocess.CalledProcessError as e:
            logging.warning(
                f"Failed to adjust position for player {player}: {e}")


def main() -> None:
    if len(sys.argv) < 2:
        logging.error("Usage: script.py <command> [args...]")
        sys.exit(1)

    command = sys.argv[1]

    if command == "position":
        if len(sys.argv) == 2:
            position = get_current_position()
            print(position)
        elif len(sys.argv) == 3:
            set_position(int(sys.argv[2]))
    elif command == "rel-position":
        if len(sys.argv) != 3:
            logging.error("rel-position requires exactly one argument")
            sys.exit(1)
        rel_position(int(sys.argv[2]))
    else:
        subprocess.run(["playerctl"] + sys.argv[1:], check=True)


if __name__ == "__main__":
    main()
