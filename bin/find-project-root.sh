#!/bin/zsh

set -e

# macOS duh...
if ! which realpath >/dev/null; then
    realpath() {
        perl -mCwd -e '$f = shift; print Cwd::realpath($f), "\n"' "$1"
        # python -c 'import os, sys; print(os.path.realpath(sys.argv[1]))' "$1"
    }
fi

find_root() {
    local filename="$1"
    local cur="$(realpath $filename:h)"
    while [[ $cur != $HOME ]]; do
        if test -d "${cur}/.git"; then echo "$cur"; break; fi
        cur="$(realpath $cur/..)"
    done
    exit 1
}

if [[ -z "$1" ]]; then
    echo "Usage: $(basename $0) <filename>"
    exit 1
fi

if [[ ! -e "$1" ]]; then
    echo "Target file does not exists: $1"
    exit 1
fi

find_root "$1"