#!/bin/sh

xrandr --output DP-1 --primary --mode 2560x1440 --pos 1920x412 --rotate normal --output HDMI-1 --mode 1920x1080 --pos 0x0 --rotate normal --output DP-2 --off --output DP-3 --off --output DP-4 --off --output DP-5 --off --output DP-6 --off --output DP-7 --off --output DP-8 --off --output DP-9 --mode 1920x1080 --pos 0x1080 --rotate normal --output DP-10 --off --output DP-11 --off

nitrogen --restore
