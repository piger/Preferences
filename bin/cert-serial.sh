#!/bin/bash

CERT="$1"

if [[ -z $CERT ]]; then
    echo "Usage: $(basename $0) <certificate file>"
    exit 1
fi

echo "ibase=16; $(openssl x509 -in "$CERT" -serial -noout | sed -e "s/^serial=//")" | bc
