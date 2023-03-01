#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit

# xrandr \
#    --output VGA-0 --off \
#    --output HDMI-0 --mode 1920x1080 --pos 4480x80 --rotate right \
#    --output DVI-D-0 --off \
#    --output DP-1-1 --primary --mode 2560x1440 --pos 1920x431 --rotate normal \
#    --output HDMI-1-1 --mode 1920x1080 --pos 0x0 --rotate normal \
#    --output DVI-D-1-1 --mode 1920x1080 --pos 0x1080 --rotate normal

screen=$(xrandr |
    sed 's/primary//' |
    awk '/ connected/ { print $1" "$3; }' |
    rofi -dmenu |
    awk '{ print $2 }')

rotate=$(echo -en "half\nnone\ncw\nccw" | rofi -dmenu)

xsetwacom list devices | grep -oE 'id: (.|..)' | cut -d' ' -f2 |
    while read -r device; do
        xsetwacom set $device MapToOutput $screen
        xsetwacom set $device Rotate $rotate
    done

notify-send -t 2000 "Set to screen $screen"

# xkbcomp ~/.config/hax-config/desktop/xkb_symbols :0

# b=$(echo -en "1.0\n0.95\n0.9\n0.85\n0.8\n0.75\n0.7\n0.65" | rofi -dmenu)

# xrandr --output HDMI-0 --brightness $b
# xrandr --output DVI-D-1-1 --brightness $b
# xrandr --output DP-1-1 --brightness $b
# xrandr --output HDMI-1-1 --brightness $b
