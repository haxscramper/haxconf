# "maim -s | xclip -selection clipboard -t image/png"
#    release+Mod2+Mod4 + c

"flameshot gui"
release+Mod2+Mod4 + c

"maim | xclip -selection clipboard -t image/png"
release+Mod2+Mod4+Control + c

"xkb-switch -s us && rofi -show drun -theme $HOME/.config/rofi-launchers/type-5/style-4.rasi"
release+Mod2+Mod4 + r

"xkb-switch -s us && rofi -show window -matching prefix"
release+Mod2+Mod4 + q

"xkb-switch -s us && rofi -show window -matching prefix"
F1

"xkb-switch -s us && rofi -show window -theme $HOME/.config/rofi-launchers/type-5/style-4.rasi"
release+Mod2+Mod4 + w

"emacsclient-window"
release+Mod2+Mod4 + e

"$HOME/.config/haxconf/scripts/rofi-select-pdf.sh"
release+Control+Mod2+Mod4 + p

"$HOME/.config/haxconf/scripts/rofi_add_strawberry_item.sh"
release+Control+Mod2+Mod4 + a

"res=$(find $HOME/defaultdirs/input -maxdepth 2 -type f \( -name "*.html" -or -name "*.txt" \) | rofi -dmenu) && [[ ! -z res ]] && firefox-developer-edition "$res""
release+Control+Mod2+Mod4 + h

"sxiv -t ~/defaultdirs/input /tmp"
release+Control+Mod2+Mod4 + s

"killall -9 xbindkeys && xbindkeys && echo [log][$(date +%T)] xbindkeys reloaded >> ~/.config/hax-local/log/$(date -I) ; notify-send -t 2000 'Reloaded xbindkeys'"
release+Control+Mod2+Mod4 + F12

"$HOME/.config/haxconf/scripts/screencast.sh"
release+Control+Mod2+Mod4 + F11

"echo '12' | nc -q0 localhost 12000"
release+Control+Mod2+Mod4 + F10

"$HOME/.config/haxconf/scripts/fixwacom.sh"
release+Mod2+Mod4 + W

"$HOME/.config/haxconf/scripts/prompt-translate.sh"
release+Mod2+Mod4 + T

"$HOME/.config/haxconf/scripts/rofi-select-song.sh"
release+Control+Mod2+Mod4 + N

"xbacklight -inc 1"
XF86MonBrightnessUp

"xbacklight -dec 1"
XF86MonBrightnessDown

"pactl set-sink-volume @DEFAULT_SINK@ +2%"
XF86AudioRaiseVolume

"pactl set-sink-volume @DEFAULT_SINK@ -2%"
XF86AudioLowerVolume

"pactl set-sink-volume @DEFAULT_SINK@ 40%"
XF86AudioStop

"strawberry --play-pause"
XF86AudioPlay

"strawberry --previous"
XF86AudioPrev

"strawberry --next"
XF86AudioNext

"copyq toggle"
release+Mod2+Mod4 + p

"xbacklight -dec 10"
XF86MonBrightnessDown

"xbacklight -inc 10"
XF86MonBrightnessUp

"~/.config/haxconf/scripts/noctrlq.sh"
Control + q + Release
