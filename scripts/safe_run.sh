#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset

lim=3000000

ulimit -v $lim
ulimit -m $lim

$@
