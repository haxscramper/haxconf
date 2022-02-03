#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit
set -x

repo_name=$HOME/backup/restic_target


restic -r $repo_name \
       --verbose \
       backup ~/defaultdirs


restic forget -r $repo_name --keep-weekly 10
restic check -r $repo_name
