#!/usr/bin/env zsh

[[ ! -d ~/.backup ]] && mkdir ~/.backup

dot_inst() {
	if [[ ! -h ${HOME}/.$1 ]]; then
		mv ${HOME}/.$1 ${HOME}/.backup/
	fi
	ln -sf ${PWD}/$1 ${HOME}/.$1
}

inst() {
	if [[ ! -h ${HOME}/$1 ]]; then
		mv ${HOME}/$1 ${HOME}/.backup/
	fi
	ln -sf ${PWD}/$1 ${HOME}/$1
}

dot_inst "screenrc"
dot_inst "zshenv"
dot_inst "zsh"
dot_inst "vimrc"
dot_inst "gvimrc"
dot_inst "vim"
dot_inst "Xdefaults"
dot_inst "hgrc"
dot_inst "gitconfig"
