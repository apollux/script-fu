#!/bin/bash

set -e

function usage {
    me=$(basename $0)
    echo "Usage: ${me} [FILE]"
    echo "  Runs 'make' when one of the files changes."
}

function snapshot_files {
    snapshot=''
    for file in $1; do
        snapshot=${snapshot}$(ls -lc --time-style=+%T:%N mugen.ml | cut -d' ' -f6),
    done
}

function check_snapshot {
    aux_snapshot=${snapshot}
    snapshot_files "$1"
    if [ "x${snapshot}" != "x${aux_snapshot}" ]; then
        snapshot=${aux_snapshot}
        return 0
    else
        return 1
    fi
}

if ! type inotifywait > /dev/null; then
    echo "The command 'inotifywait' is required but not available."
    echo "Install 'inotify-tools'."
    exit 1
fi

while getopts "h" opt; do
    case ${opt} in
        h)
            usage $0
            exit 0
            ;;
    esac
done

shift $((${OPTIND} - 1))

files="$*"

snapshot_files "${files}"
while true; do
    echo "### Building..."
    if make; then
        echo -e "### Done\n"
    else
        echo -e "### Build failed\n"
    fi

    if check_snapshot "${files}"; then
        echo "### Files changed while building"
        continue
    fi

    echo "### Waiting for filesystem changes..."
    inotifywait -q -e create -e close_write -e modify ${files}
    echo -e "### Files changed\n"
    snapshot_files "${files}"
done
