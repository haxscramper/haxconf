#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit
set -x

sudo paccache -ruk0

yaycache="$(find "$HOME"/.cache/yay -maxdepth 1 -type d | awk '{ print "-c " $1 }' | tail -n +2)"
yayremoved=$(paccache -ruvk0 $yaycache | sed '/\.cache\/yay/!d' | cut -d \' -f2 | rev | cut -d / -f2- | rev)
