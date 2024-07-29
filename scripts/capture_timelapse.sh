#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

# Create the output directory if it doesn't exist
output_dir=~/tmp/timelapse
mkdir -p "$output_dir"

# Function to capture frame using ffmpeg
capture_frame() {
    local timestamp
    timestamp=$(date +"%Y-%m-%dT%H:%M:%S.%3N") # ISO8601 timestamp with milliseconds
    ffmpeg -f v4l2 -i /dev/video4 -vframes 1 -y "${output_dir}/${timestamp}.png"
}

# Loop to capture a frame every second
while true; do
    capture_frame
    sleep 1 # Wait for 1 second
done
