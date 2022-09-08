#!/usr/bin/env bash

## DOCS: interactively select pdf file

res=$(
  find \
    $HOME/defaultdirs/automatic \
    $HOME/defaultdirs/input \
    $HOME/defaultdirs/documents \
    $HOME/.cache/telegram \
    -maxdepth 3 \
    \( -name '*.pdf' -or -name '*.djvu' \) |
    rofi -dmenu
) && [[ ! -z $res ]] && zathura "$res"
