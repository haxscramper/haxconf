#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

MOUNT_ROOT="/run/media/haxscramper"

D_MAIN="${MOUNT_ROOT}/hax_backup_main"
D_BAC1="${MOUNT_ROOT}/hax_backup_1"
D_BAC2="${MOUNT_ROOT}/hax_backup_2"


function left_right_sync() {
    LEFT="$1"
    RIGHT="$2"
    unison "${LEFT}" "${RIGHT}" -batch -debug verbose -auto -force "${LEFT}"
}

function mirror() {
    left_right_sync "${D_MAIN}/$1" "${D_BAC1}/$1"
}

left_right_sync \
    "${HOME}/defaultdirs/documents/calibre" \
    "${D_MAIN}/defaultdirs/archive/calibre_backup"

# cd "${D_MAIN}/defaultdirs/images/gallery_dl"
# ./update_gallery.sh
# cd "$SCRIPT_DIR"

# mirror "defaultdirs/images"
# mirror "defaultdirs/documents"
# mirror "defaultdirs/archive"

# unison documents_backup -batch -debug verbose -auto
