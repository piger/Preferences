# -*- shell-script -*-

typeset -U path manpath

path=( $path /sbin /bin /usr/sbin /usr/bin /usr/local/sbin /usr/local/bin )

umask 022

# avoid sourcing /etc/profile.d automatically (via /etc/zprofile or /etc/zshenv)
setopt no_GLOBAL_RCS

{
    emulate -L ksh
    setopt EXTENDED_GLOB
    # skip bash completion and /etc/profile.d/zendesk.sh (because of completion too)
    for i in /etc/profile.d/*.sh~*completion*~*zendesk.sh; do
        [[ -r $i ]] && emulate sh -c "source $i" >/dev/null
    done
    unset i
}

[[ -e ~/.zshenv.local ]] && source ~/.zshenv.local
