#!/bin/bash
# Install the latest version of Go in /opt/go

set -e

which pv >/dev/null || exit

ARCH="$(uname -m)"
OS="$(uname -s | tr '[A-Z]' '[a-z]')"

if [[ $ARCH == "x86_64" ]]; then
    ARCH="amd64"
elif [[ $ARCH == "armv7l" ]]; then
    ARCH="armv6l" # 32bit rpi4
fi

# Get the latest version available; doesn't matter here that we're grepping for "darwin-arm64"!
VERSION=$(curl -sSL https://go.dev/dl/ | perl -nl -e '/go(\d+\.\d+)(\.\d+)?\.darwin-arm64\.tar\.gz/ && print "$1$2\n"' | head -n1)

read -p "Latest version is ${VERSION}; continue? [yn] " -n 1 -r
echo

[[ $REPLY =~ ^[Yy]$ ]] || { echo "ok, nevermind"; exit; }

echo "Downloading go ${VERSION}: https://go.dev/dl/go${VERSION}.${OS}-${ARCH}.tar.gz"
curl -# -SL -O "https://go.dev/dl/go${VERSION}.${OS}-${ARCH}.tar.gz"

echo "Extracting go $VERSION in /opt/go"
sudo rm -rf /opt/go
pv "go${VERSION}.${OS}-${ARCH}.tar.gz" | sudo tar -C /opt -xzf -
rm -f "go${VERSION}.${OS}-${ARCH}.tar.gz"

if [[ "$OS" == "darwin" ]]; then
    echo "Setting up /etc/paths.d"
    if [[ ! -e /etc/paths.d/go ]]; then
        echo "/opt/go/bin" | sudo tee /etc/paths.d/go
    fi
fi

echo "Fixing permissions"
sudo find /opt/go -exec chmod ugo+r \{\} \;
sudo find /opt/go/bin -exec chmod ugo+rx \{\} \;
sudo find /opt/go -type d -exec chmod ugo+rx \{\} \;
sudo chmod o-w /opt/go

/opt/go/bin/go version
