#!/usr/bin/zsh

[[ ! -d ~/.backup ]] && mkdir ~/.backup

dot_inst() {
	if [[ ! -g ${HOME}/.$1 ]]; then
		mv ${HOME}/.$1 ${HOME}/.backup/
	fi
	ln -sf ${PWD}/$1 ${HOME}/.$1
}

inst() {
	if [[ ! -g ${HOME}/$1 ]]; then
		mv ${HOME}/$1 ${HOME}/.backup/
	fi
	ln -sf ${PWD}/$1 ${HOME}/$1
}

dot_inst "screenrc"
