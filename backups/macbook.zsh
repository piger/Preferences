#!/usr/bin/env zsh
#
# Script per il backup del macbook.
#
# Step necessari:
# - find per capire quali directory escludere
#	$ find ~ -maxdepth 4 -type d
#
# - backup separato e criptato di ~/.gnupg e ~/Work
#
# LE CAZZO DI IMMAGINI VIRTUALBOX!

# Per notare immediatamente gli errori:
setopt ERR_EXIT

# Backup criptato (chiave simmetrica) di ~/.gnupg
echo "Backup directory GPG"
duplicity --name macbook_gpg \
    -v 6 \
    /Users/sand/.gnupg \
    --allow-source-mismatch \
    scp://sand@taleggio/backup/macbook/gpg

# Backup criptato di ~/Work
echo "Backup criptato di ~/Work"
duplicity --name macbook_private \
    --encrypt-key BFAC82CE --sign-key BFAC82CE \
    -v 6 \
    /Users/sand/Work \
    --allow-source-mismatch \
    scp://sand@taleggio/backup/macbook/work

# Backup della home
echo "Backup della home"
duplicity --exclude-filelist /Users/sand/Preferences/backups/home_exclude.txt \
    --name macbook_home \
    --no-encryption \
    -v 6 \
    /Users/sand \
    --allow-source-mismatch \
    scp://sand@taleggio/backup/macbook/home

