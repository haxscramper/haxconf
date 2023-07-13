#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset

for input_file in "$@"
do
    echo "$input_file"
    output_file="${input_file%.*}"
    if [[ -e "$output_file.vtt" ]]; then
        echo "$input_file already converted to $output_file"
    else
        echo "$output_file.vtt missing, converting $input_file"
        whisper-transcribe.sh "$input_file" "$output_file"
    fi

done
