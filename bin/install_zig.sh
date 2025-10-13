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

# without 'echo', 'read' will not read the download URL... for some reason.
read LATEST_VERSION LATEST_ARCHIVE < <(echo $(curl -sSL https://ziglang.org/download/index.json | \
                                                  jq --arg os "${ARCH}-${OS}" -r '.master.version, .master[$os].tarball'))

if which zig >/dev/null && [[ $(zig version) == $LATEST_VERSION ]]; then
    echo "Already running the latest version: $LATEST_VERSION"
    exit 0
fi

echo "download ${LATEST_VERSION}: $LATEST_ARCHIVE"

curl -# -SL --output-dir /tmp -O "$LATEST_ARCHIVE"

ARCHIVE_FILENAME="/tmp/$(basename $LATEST_ARCHIVE)"

sudo rm -rf /opt/zig
sudo mkdir /opt/zig

pv "$ARCHIVE_FILENAME" | sudo tar -C /opt/zig --strip-components 1 -xzf -

rm -f "$ARCHIVE_FILENAME"

if [[ "$OS" == "darwin" ]]; then
    echo "Setting up /etc/paths.d"
    if [[ ! -e /etc/paths.d/zig ]]; then
        echo "/opt/zig" | sudo tee /etc/paths.d/zig
    fi
fi

/opt/zig/zig version
