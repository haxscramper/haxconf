#!/usr/bin/env bash

script_file="$0"
script_args="$@"

#DEPS: inotifywait,entr,sed,colecho
#DOCS:
# Userscript for qutebrowser. AUtomatically reload
# page on file change.

function run_script {
    echo "running main script $script_file"
    tmp="/tmp/$(echo $script_file | sed 's!/!_!')_script.sh"
    # Delete everything until `# --- start ---` and pipe to bash
    echo "saved copy of file to $tmp"
    sed '0,/^# --- start ---$/d' "$script_file" |
        tee $tmp | # save copy to temporary file
        bash $script_args

}

export -f run_script # Export function definition so entr can use it
export script_file   # run_script uses `script_file` and `script_args`
export script_args

echo $script_file | entr -r bash -c run_script

# main script body
# --- start ---
set errexit

function log {
    echo -- "$@" >>/tmp/qutebrowser_userscript.log
}

log "==== starting ===="
log "input url $QUTE_URL"
watched_file=$(echo $QUTE_URL | sed 's!file://!!')

if [ -f "$watched_file" ]; then
    log "watched file '$watched_file' exists"
    inotifywait -e close_write -m "$watched_file" |
        while read -r events filename; do
            command=":reload"
            log "sending $command"
            echo $command >>"$QUTE_FIFO"
        done
else
    log "watched file '$watched_file' not found"
fi

log ""
