#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

SONG=$(ncmpcpp --current-song="$HOME/defaultdirs/music/play/%D/%f" -q 2>/dev/null | tail -n1)
echo "${SONG}"
