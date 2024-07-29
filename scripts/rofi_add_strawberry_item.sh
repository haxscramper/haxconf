#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

res=$(
    find $HOME/defaultdirs/music/play \( -name '*.mp4' -or -name '*.mp3' \) |
        rofi -dmenu -i
) &&
    [[ ! -z $res ]] &&
    strawberry --append "$res"
