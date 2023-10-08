#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

# Use like normal fzf, but the output are copied to the clipboard

# https://stackoverflow.com/a/46726373/9157799
if [ -p /dev/stdin ]  # if data was piped
then
    stdin=$(</dev/stdin)
    echo "$stdin" | fzf "$@" | xclip
    fzo="$(xclip -o)"
else
    fzo=$(fzf "$@")
fi
if [ "$fzo" != "" ]
then
    export DISPLAY=:1
    echo -n "$fzo" | nohup xclip -loops 0 -sel p -sel s -sel c
    echo "$fzo"
fi
