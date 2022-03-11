#!/bin/bash
# Install the latest version of Go in /opt/go

set -e

ARCH="$(uname -m)"

VERSION=$(curl -sSL https://go.dev/dl/ | perl -nl -e '/go(\d+\.\d+\.\d+)\.darwin-arm64\.tar\.gz/ && print "$1\n"' | head -n1)

read -p "Latest version is ${VERSION}; continue? [yn] " -n 1 -r
echo

[[ $REPLY =~ ^[Yy]$ ]] || { echo "ok, nevermind"; exit; }

echo "Downloading go $VERSION"
curl -# -SL -O "https://go.dev/dl/go${VERSION}.darwin-${ARCH}.tar.gz"

echo "Extracting go $VERSION in /opt/go"
sudo rm -rf /opt/go
pv "go${VERSION}.darwin-${ARCH}.tar.gz" | sudo tar -C /opt -xzf -
rm -f "go${VERSION}.darwin-${ARCH}.tar.gz"

echo "Setting up /etc/paths.d"
if [[ ! -e /etc/paths.d/go ]]; then
    echo "/opt/go/bin" | sudo tee /etc/paths.d/go
fi

echo "Fixing permissions"
sudo find /opt/go -exec chmod ugo+r \{\} \;
sudo find /opt/go/bin -exec chmod ugo+rx \{\} \;
sudo find /opt/go -type d -exec chmod ugo+rx \{\} \;
sudo chmod o-w /opt/go

/opt/go/bin/go version
