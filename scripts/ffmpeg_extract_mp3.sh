#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset

for i in "$@"
do
    if [[ -e "${i%.*}.mp3" ]]; then
        echo "Conversion for ${i} already exists"
    else
        ffmpeg -n -i "$i" -vn -ab 192k -map 0:a "${i%.*}.mp3"
    fi
done
