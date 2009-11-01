# ========================================================
# ~/.zshenv
# Questo file verra' sempre letto da zsh e dovra' quindi
# contenere tutti le impostazioni che potrebbero influire
# sui job in crontab, su script eseguiti, etc...
# --------------------------------------------------------

ZDOTDIR=${HOME}/.zsh

# $PATH
# -----
# Le variabili path e manpath sono due array legati agli equivalenti
# $PATH e $MANPATH; in piu' con -U setto l'attributo che elimina i
# duplicati.
typeset -U path
typeset -U manpath
path=(
    $path /sbin /bin /usr/sbin /usr/bin /usr/X11R6/bin 
    /usr/games /usr/local/sbin /usr/local/bin /opt/local/bin
    /opt/bin
)

# esempio pratico di utilizzo dell'array path
[[ -d ~/bin ]] && path+=~/bin

export CVS_RSH=ssh
export RSYNC_RSH=ssh
# maledico debian e il suo chiamare firefox con il nome di ICEWEASEL.
export BROWSER="firefox"

# la mia dir Preferences
export ZPREFS=~/Preferences

#if [[ -z $SSH_CONNECTION ]]; then
#    export SCREENRC=$ZPREFS/screenrc.local
#    # ancora meglio, visto che alcuni screen ignorano $SCREENRC:
#    alias screen='screen -c .screenrc.remote'
#else
#    export SCREENRC=$ZPREFS/screenrc.remote
#fi

TIMEFMT="Real: %E User: %U System: %S Percent: %P Cmd: %J"

# Se la shell e' interattiva, usa 026.
#if [[ $- == *i* ]]; then
#	# u+rw, g+r, o+NIENTE
#	umask 026
#else
#	umask 022
#fi

umask 022

# ulimit ?

# vim: ft=zsh
