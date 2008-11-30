#!/bin/sh

git archive --format=tar --prefix=Preferences/ HEAD | gzip -c > Pref-$(date "+%F").tar.gz
