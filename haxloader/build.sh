#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit

function gdb_cmd {
    gdb                                           \
        -batch                                    \
        -ex "set print address off"               \
        -ex "set print frame-arguments presence"  \
        -ex "run"                                 \
        -ex "bt"                                  \
        --args $@
}

nim c daily_note.nim
nim c -o=haxloader.so haxloader.nim
emacs -Q --batch --directory "$PWD" -l main.el
