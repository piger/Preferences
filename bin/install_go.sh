#!/bin/bash
# Install the latest version of Go in /opt/go.

set -e

which pv >/dev/null || exit

ARCH="$(uname -m)"
OS="$(uname -s | tr '[A-Z]' '[a-z]')"

if [[ $ARCH == "x86_64" ]]; then
    ARCH="amd64"
elif [[ $ARCH == "armv7l" ]]; then
    ARCH="armv6l" # 32bit rpi4
elif [[ $ARCH == "aarch64" ]]; then
    ARCH="arm64"
fi

# LATEST will contain a string like "go1.19.4".
LATEST="$(curl -sSL 'https://go.dev/VERSION?m=text')"
# INSTALLED will contain a string similar to $LATEST, but containing the installed version.
INSTALLED="$(go version | awk '{ print $3 }')"

if [[ "$LATEST" == "$INSTALLED" ]]; then
    echo "Latest version $LATEST already installed."
    exit
fi

# VERSION contains the numerical part of a Go version; for example "go1.19.4" is "1.19.4".
VERSION="${LATEST#go}"

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
