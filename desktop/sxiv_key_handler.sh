#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

notify() {
    notify-send "$@"
}

notify "Test"

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
            if [[ "${file##*.}" -eq "gif" ]]; then
                echo -n "$abs_path" | xclip -selection clipboard
                notify "Cannot copy gif to clibpoard.\n Copied path instead"
            else
                xclip -sel cli -t image/png -i "$abs_path"
                notify "Copied image $abs_path"
            fi
            ;;
        "C-d")
            mv "$file" /tmp
            ;;
    esac
done
