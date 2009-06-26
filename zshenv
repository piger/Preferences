# ========================================================
# ~/.zshenv
# Questo file verra' sempre letto da zsh e dovra' quindi
# contenere tutti le impostazioni che potrebbero influire
# sui job in crontab, su script eseguiti, etc...
# --------------------------------------------------------

ZDOTDIR=${HOME}/.zsh

# $PATH
# -----
declare -U path		# -U purga i duplicati
path=( $path[@] /sbin /bin /usr/sbin /usr/bin /usr/X11R6/bin 
		/usr/games /usr/local/sbin /usr/local/bin /opt/local/bin /opt/bin
)

[[ -d ~/bin ]] && path=( $path[@] ~/bin )

export CVS_RSH=ssh
export RSYNC_RSH=ssh
# maledico debian e il suo chiamare firefox con il nome di ICEWEASEL.
export BROWSER="firefox"

TIMEFMT="Real: %E User: %U System: %S Percent: %P Cmd: %J"

# Se la shell e' interattiva, usa 026.
if [[ $- == *i* ]]; then
	# u+rw, g+r, o+NIENTE
	umask 026
else
	umask 022
fi

# ulimit ?

# Non va bene, da rifare.
# export TSOCKS_CONF_FILE=${HOME}/tsocks.conf

# OS X Leopard gestisce ssh-agent tramite launchd e lo collega
# al Keychain, quindi non c'e' bisogno di usare keychain di gentoo.
# GNOME invece usa seahorse-agent.
# Quindi... keychain di gentoo... VAFFANCULO.
#
# if [[ -z $SSH_AUTH_SOCK ]]; then
# 	if (( $+commands[keychain] )); then
# 		keychain -q
# 		[[ -f ${HOME}/.keychain/${HOST}-sh ]] &&
# 		. ${HOME}/.keychain/${HOST}-sh
# 		[[ -f ${HOME}/.keychain/${HOST}-sh-gpg ]] &&
# 		. ${HOME}/.keychain/${HOST}-sh-gpg
# 	fi
# fi

# vim: ft=zsh
