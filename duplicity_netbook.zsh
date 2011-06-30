#!/usr/bin/env zsh

# Backup per la home del netbook aziendale.
#     --gpg-options "--compress-algo=bzip2" \

set -e

do_backup() {
    duplicity \
        --exclude-globbing-filelist ~/tuttedir.txt \
        --name home_daily \
        --encrypt-key D3B75D54 \
        --sign-key D3B75D54 \
        -v 6 \
        /home/sand \
        scp://sand@taleggio/backup/hal/
}

do_status() {
    duplicity \
        collection-status \
        --name home_daily \
        --encrypt-key D3B75D54 \
        --sign-key D3B75D54 \
        -v 6 \
        scp://sand@taleggio/backup/hal/
}

do_backup
