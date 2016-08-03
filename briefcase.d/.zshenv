# -*- shell-script -*-

typeset -U path manpath

path=( $path /sbin /bin /usr/sbin /usr/bin /usr/local/sbin /usr/local/bin )

umask 022

[[ -e ~/.zshenv.local ]] && source ~/.zshenv.local
