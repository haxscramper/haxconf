#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

git stash || true
git pull origin master
./link.sh
