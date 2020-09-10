#!/bin/bash
# macOS specific

CERT="$1"
KEY="$2"

if [[ -z $CERT || -z $KEY ]]; then
    echo "Usage: $(basename $0) <certificate file> <key file>"
    exit 1
fi

cert_sha=$(openssl x509 -in "$CERT" -pubkey -noout -outform pem | shasum -a 256)
key_sha=$(openssl pkey -in "$KEY" -pubout -outform pem | shasum -a 256)

if [[ $cert_sha != $key_sha ]]; then
    echo "Certificate do not match the private key"
    exit 1
else
    echo "OK"
    exit 0
fi
