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

if [[ -z $4 ]]; then
    MODEL="base"
else
    MODEL="$4"
fi

input_file=$(realpath "$1")
output_file=$(realpath "$2")
temp_file=$(mktemp).wav

whisper_transcribe.py \
    transcribe \
    "$input_file" \
    --csv-path "${output_file}.csv" \
    --vtt-path "${output_file}.vtt" \
    --language "$LANG" \
    --model "$MODEL"

