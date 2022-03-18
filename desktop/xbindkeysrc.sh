# "maim -s | xclip -selection clipboard -t image/png"
#    release+Mod2+Mod4 + c


"flameshot gui"
    release+Mod2+Mod4 + c

"maim | xclip -selection clipboard -t image/png"
    release+Mod2+Mod4+Control + c

"xkb-switch -s us && rofi -show run"
    release+Mod2+Mod4 + r


"emacsclient-window"
    release+Mod2+Mod4 + e

"$HOME/.config/hax-config/scripts/rofi-select-pdf.sh"
    release+Control+Mod2+Mod4 + p

"res=$(find $HOME/defaultdirs/input -maxdepth 2 -type f \( -name "*.html" -or -name "*.txt" \) | rofi -dmenu) && [[ ! -z res ]] && firefox-developer-edition "$res""
    release+Control+Mod2+Mod4 + h

"sxiv -t ~/defaultdirs/input "
    release+Control+Mod2+Mod4 + s

"killall -9 xbindkeys && xbindkeys && echo [log][$(date +%T)] xbindkeys reloaded >> ~/.config/hax-local/log/$(date -I) ; notify-send -t 2000 'Reloaded xbindkeys'"
    release+Control+Mod2+Mod4 + F12

"$HOME/.config/hax-config/scripts/screencast.sh"
    release+Control+Mod2+Mod4 + F11

"echo '12' | nc -q0 localhost 12000"
    release+Control+Mod2+Mod4 + F10


"$HOME/.config/hax-config/scripts/fixwacom.sh"
    release+Mod2+Mod4 + W

"$HOME/.config/hax-config/scripts/rofi-select-song.sh"
    release+Control+Mod2+Mod4 + N

"pactl set-sink-volume @DEFAULT_SINK@ +1%"
    XF86AudioRaiseVolume

"pactl set-sink-volume @DEFAULT_SINK@ -1%"
    XF86AudioLowerVolume

"pactl set-sink-volume @DEFAULT_SINK@ 0%"
    XF86AudioStop

"mpc toggle"
    XF86AudioPlay

"mpc prev"
    XF86AudioPrev

"mpc next"
    XF86AudioNext

"~/.config/hax-config/scripts/copyq_start.nim.bin"
    release+Mod2+Mod4 + p

"xbacklight -dec 10"
   XF86MonBrightnessDown

"xbacklight -inc 10"
   XF86MonBrightnessUp

"~/.config/hax-config/scripts/noctrlq.sh"
   Control + q + Release

