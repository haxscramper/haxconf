#!/usr/bin/env bash
# -*- coding: utf-8 -*-
s~/.config/haxconf/desktop/sxiv_key_handler.shet -o nounset
set -o errexit

notify() {
    notify-send "$@"
}

while read file; do
    case "$1" in
        "r")
            convert -rotate 90 "$file" "$file"
            ;;
        "R")
            convert -rotate -90 "$file" "$file"
            ;;
        "f")
            convert -flop "$file" "$file"
            ;;
        "Y")
            abs_path="$(realpath $file)"
            notify "Copied path to $abs_path"
            echo -n "$abs_path" | xclip -selection clipboard
            ;;
        "y")
            abs_path="$(realpath $file)"
            notify "Copied $abs_path"
            xclip -sel cli -t image/png -i "$abs_path"
            ;;
        "C-d")
            mv "$file" /tmp
            ;;
    esac
done
