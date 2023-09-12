#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

selection=$(selector_history.py get "$1" | fzf -i)
selector_history.py store "$1" "$selection"
echo "$selection"
