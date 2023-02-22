#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

sanitize() {
    shopt -s extglob

    filename=$(basename "$1")
    directory=$(dirname "$1")

    filename_clean=$(
        echo -n $filename |
            sd '^\s*' '' | sd '\.(\w+)$' 'XXXXX$1' | sd '\W+' '_' | sd 'XXXXX(\w+)$' '.$1'
    )
    if (test "$filename" != "$filename_clean"); then
        mv -v --backup=numbered "$1" "$directory/$filename_clean"
    fi
}

export -f sanitize
find $1 -depth -exec bash -c 'sanitize "$0"' {} \;
