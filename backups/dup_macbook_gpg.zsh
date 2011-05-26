#!/usr/bin/env zsh
#
# 26 Maggio 2011
#
# Backup della directory ~/.gnupg con chiave simmetrica.

source ./common.zsh
setopt ERR_EXIT

duplicity --name macbook_gpg \
    $VERBOSITY \
    /Users/sand/.gnupg \
    $DEST_MACBOOK_GPG
