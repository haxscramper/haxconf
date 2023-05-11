#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

lim=3000000

ulimit -v $lim
ulimit -m $lim

rr record $@
rr replay -s 5050 -k
