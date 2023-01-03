#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

sanitize() {
shopt -s extglob;

filename=$(basename "$1")
directory=$(dirname "$1")

filename_clean="${filename//+([^[:alnum:]_-\.])/_}"

if (test "$filename" != "$filename_clean")
then
mv -v --backup=numbered "$1" "$directory/$filename_clean"
fi
}


export -f sanitize
find $1 -depth -exec bash -c 'sanitize "$0"' {} \;
