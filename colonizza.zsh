#!/usr/bin/env zsh
# Finalmente, l'impero è arrivato.
# 
#                 _                                       |
#                ;P'                         .........,-- |
#                8P                                       |
#              ,8P _,-....__                         __    \_
#             o8'-'     o.  `-.._              ,o--''        ._
#           ,dP'        `YYbooo__`'`--------=           .      `-..__,,.-
#          ,88             _[8888P        _,'         ,'
#         d88'        _,oo8PP''         Y`       _,,-'       /
#        ,88/      ,d88"'             b-'        '          ,'      |
#        88/,b-'`>. '`YYbooo__       ./                    /       |
#       ]8['       `..   `'`"PP8    ,-                   ,'        |
#       ]8[           `-.__         \                   /         .'
#       d8|                `''-------`
#      ]8[
#      ,8.                    'finalmente una configurazione seria!'
#      `P'
#   ,dFcccd_.._oo._      "madonna!"
# cSFb888P8j8888?88b               'CRISTO, ERA ORA!'     "DAJE Zì!"
# J888888888888Ybo?$8o.
#  'i;::'""F?hc8c888d$8b.
# ''           `''`"?FF$88cccccccccccccccc.__
# Il colonizzatore.     `'YP''''''''''`-::;;?F?hccc.
# Colui che ficca la configurazione nel          '''`-..._
# posto giusto, facendo dei backup e usando i colori      ``-..___
# per farti capire quando iniziare a strippare.                   `''
# 
# <<Il colonizzatore non è un coione e nfatti usa i symlink verso 'sta directory co
# tutti i puntofile, e si trova un file co 'o stesso nome je accolla n'altro nome
# de backup e se lo leva dar cazzo.>>
#          -- Anonimo

# Ispirato da https://github.com/sjl/dotfiles/blob/master/bin/bootstrap.sh che
# pero' fa il matto con zsh e poi COME TUTTI non usa manco mezza feature.
#
# Last update: 5/9/2012
function colonizza() {
	[[ -h "$HOME/$2" ]] || ln -s "$HOME/Preferences/$1" "$HOME/$2"
}

colonizza "vim"			".vim"
colonizza "vimrc"		".vimrc"
colonizza "gvimrc"		".gvimrc"
colonizza "ackrc"		".ackrc"
colonizza "pythonrc.py"	".pythonrc.py"
colonizza "gitconfig"	".gitconfig"
colonizza "zshenv"		".zshenv"
colonizza "zsh"			".zsh"
colonizza "tmux.conf"	".tmux.conf"
colonizza "lftprc"		".lftprc"
colonizza "taskrc"		".taskrc"
