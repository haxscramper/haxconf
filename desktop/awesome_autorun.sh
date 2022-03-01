#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset

function run {
  if ! pgrep -f $1 ;
  then
    $@&
  fi
}

# run copyq

# xset s off
# xset -dpms
# xset s noblank

# setxkbmap -layout us,ru
# setxkbmap -option 'grp:win_space_toggle' -option ctrl:nocaps

# xsetwacom list devices | grep -oE 'id: (.|..)' | cut -d' ' -f2 |
#   while read -r device
#   do
#     xsetwacom set $device MapToOutput 2560x1440+1920+408
#     xsetwacom set $device Rotate half
#   done
