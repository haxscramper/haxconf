#!/usr/bin/env bash

selected=$(mpc playlist | cat -n | rofi -i -dmenu)
if [ -z "$selected" ]; then
    notify-send "No selection"
else
    notify-send "Selected '$selected'"
    mpc insert $(echo $selected | awk '{print $1}')
fi
