#!/usr/bin/env bash

# Disable screensaver
xset s off
xset -dpms
xset s noblank

xbindkeys # Shortcuts
copyq &   # Clibpard manager
flameshot &
#picom &

xkbcomp ~/.config/.xkb_symbols :0

xsetwacom list devices | grep -oE 'id: (.|..)' | cut -d' ' -f2 |
    while read -r device; do
        xsetwacom set $device MapToOutput 2560x1440+1920+431
        xsetwacom set $device Rotate half
    done

exec awesome # Launch window manager
# exec startxfce4

# export DESKTOP_SESSION=plasma
# exec startplasma-x11
