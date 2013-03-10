#!/bin/sh

TODAY=$(date +"%F")

git archive --format=tar --prefix=Preferences-${TODAY}/ HEAD | gzip -c > Preferences-${TODAY}.tar.gz
