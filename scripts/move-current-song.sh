#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

PLAY="$HOME/defaultdirs/music/play"
cd $PLAY
selected=$(fd -t d | sed 's!^\./!!' | rofi -dmenu)
file=$(get-current-song.sh)
notify-send "Moving '$file' to '$PLAY/$selected'"
mv "$file" "$PLAY/$selected"
