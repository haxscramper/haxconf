#!/usr/bin/env bash

# Disable screensaver
xset s off
xset -dpms
xset s noblank

xbindkeys                                 # Shortcuts
$HOME/workspace/git-sandbox/CopyQ/copyq & # Clibpard manager
flameshot &
#picom &

xkbcomp ~/.config/.xkb_symbols :0

xsetwacom list devices | grep -oE 'id: (.|..)' | cut -d' ' -f2 |
    while read -r device; do
        xsetwacom set $device MapToOutput 2560x1440+1920+431
        xsetwacom set $device Rotate half
    done

xrandr \
    --output VGA-0 --off \
    --output HDMI-0 --mode 1920x1080 --pos 4480x80 --rotate right \
    --output DVI-D-0 --off \
    --output DP-1-1 --primary --mode 2560x1440 --pos 1920x431 --rotate normal \
    --output HDMI-1-1 --mode 1920x1080 --pos 0x0 --rotate normal \
    --output DVI-D-1-1 --mode 1920x1080 --pos 0x1080 --rotate normal

exec awesome # Launch window manager
# exec startxfce4

# export DESKTOP_SESSION=plasma
# exec startplasma-x11
