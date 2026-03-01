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

prefix="/usr"
exec_prefix="${prefix}"
xrdb="xrdb"
xinitdir="/etc/X11/xinit"
# xclock="xclock"
# xterm="xterm"
twm="twm"
xmodmap="xmodmap"

userresources="$HOME/.Xresources"
usermodmap="$HOME/.Xmodmap"
sysresources="$xinitdir/.Xresources"
sysmodmap="$xinitdir/.Xmodmap"

# merge in defaults and keymaps

if [ -f "$sysresources" ]; then
    if [ -x /usr/bin/cpp ] ; then
        "$xrdb" -merge "$sysresources"
    else
        "$xrdb" -nocpp -merge "$sysresources"
    fi
fi

if [ -f "$sysmodmap" ]; then
    "$xmodmap" "$sysmodmap"
fi

if [ -f "$userresources" ]; then
    if [ -x /usr/bin/cpp ] ; then
        "$xrdb" -merge "$userresources"
    else
        "$xrdb" -nocpp -merge "$userresources"
    fi
fi

if [ -f "$usermodmap" ]; then
    "$xmodmap" "$usermodmap"
fi

# start some nice programs

if [ -d "$xinitdir"/xinitrc.d ] ; then
	for f in "$xinitdir/xinitrc.d"/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

"$twm" &
# "$xclock" -geometry 50x50-1+1 &
# "$xterm" -geometry 80x50+494+51 &
# "$xterm" -geometry 80x20+494-0 &
# exec "$xterm" -geometry 80x66+0+0 -name login

exec awesome # Launch window manager
# exec startxfce4

# export DESKTOP_SESSION=plasma
# exec startplasma-x11
