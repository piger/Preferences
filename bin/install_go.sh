#!/usr/bin/env bash
# Install the latest version of Go in /opt/go.

set -ueo pipefail

# installed checks if $1 is an existing command.
installed() {
    local cmd
    cmd=$(command -v "${1}")
    [[ -n "${cmd}" ]] && [[ -f "${cmd}" ]]
    return ${?}
}

# fatal prints its arguments as a fatal error.
fatal() {
    >&2 echo "ERROR: ${*}"
    exit 1
}

# check required dependencies.
deps=(tr curl pv sudo awk)
for dep in "${deps[@]}"; do
    installed "${dep}" || fatal "missing required command: '${dep}'"
done

# determine architecture and OS.
ARCH="$(uname -m)"
OS="$(uname -s | tr 'A-Z' 'a-z')"

if [[ $ARCH == "x86_64" ]]; then
    ARCH="amd64"
elif [[ $ARCH == "armv7l" ]]; then
    ARCH="armv6l" # 32bit rpi4
elif [[ $ARCH == "aarch64" ]]; then
    ARCH="arm64"
fi

read -r FILENAME LATEST < <(curl -s 'https://go.dev/dl/?mode=json' | jq --arg os "$OS" --arg arch "$ARCH" -r '.[0] | .files[] | select((.os == $os) and (.arch == $arch) and (.kind == "archive")) | "\(.filename) \(.version)"')

# VERSION contains the numerical part of a Go version; for example "go1.19.4" is "1.19.4".
VERSION="${LATEST#go}"

if installed go; then
    # INSTALLED will contain a string similar to $LATEST, but containing the installed version.
    INSTALLED="$(go version | awk '{ print $3 }')"

    if [[ "$LATEST" == "$INSTALLED" ]]; then
        echo "Latest version $LATEST already installed."
        exit
    fi

    read -p "Latest version is ${VERSION}; continue? [yn] " -n 1 -r
    echo

    [[ $REPLY =~ ^[Yy]$ ]] || { echo "ok, nevermind"; exit; }
fi

if [[ -d /opt/go ]]; then
    echo "Deleting the existing installation in /opt/go"
    sudo rm -rf /opt/go
fi

echo "Downloading go ${VERSION}: https://go.dev/dl/${FILENAME}"
curl -fsSL -o- "https://go.dev/dl/${FILENAME}" \
     | pv | sudo tar -C /opt -xzf -

if [[ "$OS" == "darwin" ]]; then
    echo "Setting up /etc/paths.d"
    if [[ ! -e /etc/paths.d/go ]]; then
        echo "/opt/go/bin" | sudo tee /etc/paths.d/go
    fi
fi

/opt/go/bin/go version
