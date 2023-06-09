#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

branch="$(git rev-parse --abbrev-ref HEAD)"

if [[ "$branch" == "dev*" ]]; then
  echo "Dev Branch commit is blocked"
  exit 1
fi

if [[ "$branch" == "master" ]]; then
  echo "Master Branch commit is blocked"
  exit 1
fi
