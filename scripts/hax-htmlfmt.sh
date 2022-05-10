#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset

# Because warnings are apparently errors that doom formatter cannot
# properly process.

tidy --tidy-mark no -i 4 -w 80 -m -quiet --force-output y -wrap 0 2>/dev/null || true

# xmllint --html --format --pretty 2 -
