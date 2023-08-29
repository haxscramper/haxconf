#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

whisper-transcribe-all.sh *.mkv *.mp4 &
ffmpeg_extract_mp3.sh *.mkv *.mp4 &
