#!/usr/bin/env zsh
# https://mastodon.nl/@miekg/116896345579165431
# https://groups.google.com/g/golang-nuts/c/tVWOr0zXF08/m/Bq1HaTBpCQAJ
# https://gist.github.com/JetSetIlly/9147e371074734aac7fe05c801b457d9

if [[ $# -ne 0 ]]; then
    echo "no arguments supported. read from stdin only"
	exit 1
fi

GO="package main

func main() {
  $(cat -)
}
"

GOFILE=$(mktemp).go
trap "rm $GOFILE" EXIT
echo $GO | goimports > $GOFILE && go run $GOFILE
