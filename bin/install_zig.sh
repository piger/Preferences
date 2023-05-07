#!/bin/bash
# NOTE: this is an early version. Consider it untested.

set -eu

ARCH="$(uname -m)"

if [[ $ARCH == "arm64" ]]; then
    ARCH="aarch64"
fi

OS="$(uname -s)"

if [[ $OS == "Darwin" ]]; then
    OS="macos"
fi

curl -sSL -o /tmp/zig-index.json https://ziglang.org/download/index.json

LATEST_VERSION="$(jq -r '.master.version' /tmp/zig-index.json)"
LATEST_ARCHIVE="$(jq --arg os ${ARCH}-${OS} -r '.master[$os].tarball' /tmp/zig-index.json)"

echo "download ${LATEST_VERSION}: $LATEST_ARCHIVE"

curl -# -SL --output-dir /tmp -O "$LATEST_ARCHIVE"

sudo rm -rf /opt/zig
sudo mkdir /opt/zig

pv "/tmp/zig-${OS}-${ARCH}-${LATEST_VERSION}.tar.xz" | sudo tar -C /opt/zig --strip-components 1 -xzf -

rm -f "/tmp/zig-index.json" "/tmp/zig-${OS}-${ARCH}-${LATEST_VERSION}.tar.xz"

if [[ "$OS" == "darwin" ]]; then
    echo "Setting up /etc/paths.d"
    if [[ ! -e /etc/paths.d/zig ]]; then
        echo "/opt/zig" | sudo tee /etc/paths.d/zig
    fi
fi

/opt/zig/zig version
