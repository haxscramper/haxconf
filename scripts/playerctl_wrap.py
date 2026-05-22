#!/usr/bin/env python
import argparse
import re
import subprocess
import sys
from typing import Optional


def get_all_players() -> list[str]:
    result = subprocess.run(
        ["playerctl", "--list-all"],
        capture_output=True,
        text=True,
        check=True,
    )
    return [
        line.strip() for line in result.stdout.splitlines() if line.strip()
    ]


def get_playing_players(
        window_pattern: Optional[re.Pattern[str]] = None) -> list[str]:
    result = subprocess.run(
        ["playerctl", "--all-players", "status"],
        capture_output=True,
        text=True,
        check=True,
    )
    status_lines = [
        line.strip() for line in result.stdout.splitlines() if line.strip()
    ]
    player_names = get_all_players()

    playing_players = []
    for i, status in enumerate(status_lines):
        if status == "Playing" and i < len(player_names):
            player = player_names[i]
            if window_pattern is None or window_pattern.search(player):
                playing_players.append(player)

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


def rel_position(offset: int,
                 window_pattern: Optional[re.Pattern[str]] = None) -> None:
    playing_players = get_playing_players(window_pattern)

    if not playing_players:
        print("No matching players are currently playing", file=sys.stderr)
        sys.exit(1)

    for player in playing_players:
        current_pos = get_current_position(player)
        new_pos = current_pos + offset
        set_position(new_pos, player)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(add_help=False)
    parser.add_argument("--window", dest="window", default=None)
    parser.add_argument("rest", nargs=argparse.REMAINDER)
    args = parser.parse_args()

    if not args.rest:
        print("Usage: script.py [--window PATTERN] <command> [args...]",
              file=sys.stderr)
        sys.exit(1)

    return args


def main() -> None:
    args = parse_args()
    command = args.rest[0]
    command_args = args.rest[1:]

    window_pattern = None
    if args.window is not None:
        window_pattern = re.compile(args.window, re.IGNORECASE)

    if command == "position":
        if not command_args:
            position = get_current_position()
            print(position)
        elif len(command_args) == 1:
            set_position(int(command_args[0]))
        else:
            print("position accepts at most one argument", file=sys.stderr)
            sys.exit(1)
    elif command == "rel-position":
        if len(command_args) != 1:
            print("rel-position requires exactly one argument",
                  file=sys.stderr)
            sys.exit(1)
        rel_position(int(command_args[0]), window_pattern)
    else:
        if window_pattern is not None:
            matching_players = [
                player for player in get_all_players()
                if window_pattern.search(player)
            ]
            if not matching_players:
                print("No matching players found", file=sys.stderr)
                sys.exit(1)

            subprocess.run(
                ["playerctl", "--player", ",".join(matching_players), command]
                + command_args,
                check=True,
            )
        else:
            subprocess.run(["playerctl"] + args.rest, check=True)


if __name__ == "__main__":
    main()
