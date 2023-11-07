#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o errexit
set -x

if [[ -z $1 ]]; then
    echo "Input and output files are required"
    exit 1
fi

if [[ -z $2 ]]; then
    echo "Input and output files are required"
    exit 1
fi

if [[ -z $3 ]]; then
    LANG="en"
else
    LANG="$3"
fi


input_file=$(realpath "$1")
output_file=$(realpath "$2")
temp_file=$(mktemp).wav

cd ~/software/whisper.cpp
ffmpeg -i "$input_file" -acodec pcm_s16le -ac 2 -ar 16000 "$temp_file"
./main \
    --print-progress \
    --language "$LANG" \
    --output-file "$output_file" \
    --output-vtt \
    --output-csv \
    --print-colors \
    "$temp_file"
