#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit

xclip -sel cli -t image/png -o > \
    test.png && cuneiform -l eng -f text -o out.txt test.png > \
    /dev/null && cat out.txt | xclip -sel cli

notify-send -t 2000 'Converted image to text'
