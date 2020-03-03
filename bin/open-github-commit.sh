#!/bin/bash

if [[ -z "$1" ]]; then
    echo "Usage: $(basename $0) <github commit hash>"
    exit 1
fi

# $1 = <commit hash>
open $(git remote -v | \
           awk -v hash="$1" \
               '/origin.*\(fetch\)/ { sub(":", "/", $2); gsub("^git@", "", $2); gsub("\.git$", "", $2); print "https://" $2 "/commit/" hash }')
