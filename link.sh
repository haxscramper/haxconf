#!/usr/bin/env bash

git submodule update --init --recursive

dotbot \
    --force-color \
    --plugin external/dotbot-git/git.py \
    -c install.conf.yaml
