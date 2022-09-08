#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset

echo "executing cloning"
for group in $HOME/defaultdirs/repos/*; do
    for repo in $group/*; do
        if [[ -e "$repo/.git" ]]; then
            git clone $repo
        fi
    done
done
echo "cloning completed"
