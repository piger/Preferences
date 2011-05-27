#!/usr/bin/env zsh
#
# 26 Maggio 2011
#
# Variabili di uso comune per gli script di backup

# La chiave da utilizzare per sign e encrypt.
GPG_IDENTITY_ENC="BFAC82CE"
GPG_IDENTITY_SIGN=$GPG_IDENTITY_ENC

# Command line duplicity per sign e encrypt.
DUP_ENCRYPT="--encrypt-key ${GPG_IDENTITY_ENC} --sign-key ${GPG_IDENTITY_SIGN}"

# Verbosity per duplicity e rdiff-backup
VERBOSITY="-v 6"

# Destinazioni per i backup
DESTINATION_ROOT="scp://sand@taleggio/backup"

# Macbook - gpg
DEST_MACBOOK_GPG="${DESTINATION_ROOT}/macbook/gpg"
DEST_MACBOOK_WORK="${DESTINATION_ROOT}/macbook/work"
DEST_MACBOOK_HOME="${DESTINATION_ROOT}/macbook/home"
