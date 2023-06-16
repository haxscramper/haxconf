#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset

for i in "$@"
do
    ffmpeg -n -i "$i" -vn -ab 192k -map 0:a "${i%.*}.mp3"
done
