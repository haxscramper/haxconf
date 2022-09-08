#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
# set -o errexit

pids=$(xdotool search --class "kitty")
for pid in $pids; do
    name=$(xdotool getwindowname $pid)
    notify-send "Sending <space> to window '$name' (id: $pid)"
    xdotool sleep 0.3 key --clearmodifiers --window $pid space
    notify-send "Done"
done
