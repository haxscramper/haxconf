#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

if command -v elvish &> /dev/null
then
    exec elvish
    exit
fi

if command -v fish &> /dev/null
then
    exec fish
    exit
fi

exec bash
