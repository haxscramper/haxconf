#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

ffmpeg_extract_mp3.sh *.mkv *.mp4
whisper-transcribe-all.sh *.mkv *.mp4
