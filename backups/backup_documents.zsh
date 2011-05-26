#!/bin/zsh
#

tarsnap -cv -f backup-Documents_$(date "+%d-%m-%y") ${HOME}/Documents
