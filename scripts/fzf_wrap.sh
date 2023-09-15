#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit
set -o xtrace

selection=$(selector_history.py get "$@" | fzf -i)
selector_history.py store "$1" "$selection"
echo "$selection"
