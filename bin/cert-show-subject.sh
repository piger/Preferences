#!/bin/bash

CERT="$1"

if [[ -z $CERT ]]; then
    echo "Usage: $(basename $0) <certificate file>"
    exit 1
fi

openssl x509 -noout -subject -in "$CERT"
