#!/usr/bin/zsh

[[ ! -d ~/.backup ]] && mkdir ~/.backup

suca() {
	if [[ ! -h ~/$1 ]]; then
		mv ~/$1 ~/.backup
		ln -s $1
}

suca
