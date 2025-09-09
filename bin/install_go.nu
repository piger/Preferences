#!/usr/bin/env nu

# Prompt the user with message and a yes/no selection. Returns true or false based on the selection.
def prompt [message] {
    ["no" "yes"]
    | input list $message
    | match $in {
        "yes" => true,
        _ => false,
    }
}

# set devel to true to read a local file instead of the actual download URL
let devel = false

let release = if $devel {
    open ($env.HOME | path join "go.json") | where stable == true | first
} else {
    http get "https://go.dev/dl/?mode=json" | where stable == true | first
}

# get the current installed version of Go
let current_version = if (which go | is-empty) {
    null
} else {
    # go version => "go version go1.25.1 darwin/arm64"
    ^go version | split row " " | $in.2
}

print $"Latest version: ($release.version)"

match $current_version {
    null => "not installed",
    _ => $current_version,
} | print $"Current version: ($in)"

if $release.version == $current_version {
    exit
}

if not (prompt "Select yes to continue") {
    exit
}

let os = uname | get operating-system | str downcase
let arch = uname | get machine

let download = $release
| get files
| where os == $os and arch == $arch and kind == "archive"
| first

# trigger a sudo prompt
sudo true

print "Deleting existing installation in /opt/go"
sudo rm -rf /opt/go

print $"Downloading https://go.dev/dl/($download.filename) ($download.size | into filesize)"

(curl --progress-bar --location $"https://go.dev/dl/($download.filename)"
     | sudo tar -C /opt -xzf -)

if $os == "darwin" {
    print "Setting up /etc/paths.d"
    echo "/opt/go/bin" | sudo tee /etc/paths.d/go out> /dev/null
}

/opt/go/bin/go version

print "Install or upgrade Go tools: gopls, staticcheck, panicparse, delve."
/opt/go/bin/go install golang.org/x/tools/gopls@latest
/opt/go/bin/go install honnef.co/go/tools/cmd/staticcheck@latest
/opt/go/bin/go install github.com/maruel/panicparse/v2@latest
/opt/go/bin/go install github.com/go-delve/delve/cmd/dlv@latest
