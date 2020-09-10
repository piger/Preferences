#!/bin/bash

CHAIN="$1"

if [[ -z $CHAIN ]]; then
    echo "Usage: $(basename $0) <certificate file>"
    exit 1
fi

awk 'BEGIN {c=0;} /BEGIN CERT/{c++} { print > "cert." c ".pem"}' "$CHAIN"
