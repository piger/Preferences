# Preferences.git -- i miei file di configurazione per la command line.

My collection of dotfiles.

## Installation

Just run `./colonizza.py`, or edit `dotfiles.cfg` for customization.

## Submodules

Installazione submodules git:

	$ git submodule add <URL> <path>

Ad esempio:

	$ git submodule add http://github.com/sjl/gundo.vim.git ~/.vim/bundle/Gundo

Ricordarsi di fare il commit con il file `.gitmodules` aggiornato, e negli altri
repo fare:

	$ git submodule update --init

Quando si vuole aggiornare i repository dei submodule si può usare il comando:

	$ git submodule foreach git pull origin master

Se questo comando fetcha nuove versioni dei submodule, `git status`
mostrerà `(new commits)` accanto ai loro nomi; questi cambiamenti
possono essere committati. Gli altri utenti di questo repository
dovranno aggiornare eseguendo prima un `git pull` e poi il `git
submodule foreach` descritto qui sopra, altrimenti saranno fuori sync.

Puo' capitare che `git status` mostri output tipo:

	#       modified:   vim/bundle/FuzzyFinder (untracked content)
	#       modified:   vim/bundle/L9 (untracked content)

Il motivo e' che la funzione `Helptags()` di `pathogen` crea i file di help per
vim dentro le directory dei singoli bundle, percio' git segnala che ci sono
cambiamenti non tracciati; entrando infatti nella directory `vim/bundle/L9` ed
eseguendo `git status` vedremmo come i file non tracciati sarebbero:

	# On branch master
	# Untracked files:
	#   (use "git add <file>..." to include in what will be committed)
	#
	#       doc/tags
	#       doc/tags-ja

L'unica soluzione **sporca** al momento e' quella di dire a git di **ignorare**
gli *untracked content*:

	[submodule "vim/bundle/nerdcommenter"]
		path = vim/bundle/nerdcommenter
		url = http://github.com/scrooloose/nerdcommenter.git
		ignore = untracked 

Oppure con un one-liner:

	for s in `git submodule  --quiet foreach 'echo $name'` ; do git config submodule.$s.ignore untracked ; done


## Vim

Lista di plugin che potrebbero essere interessanti ma non mi servono e/o non
utilizzo mai:

- Extradite: un browser di history alla "tig"

## Git

Su questo articolo di github dicono che le versioni recenti (> 1.6) di git
includono un helper per integrarsi con keychains di OS X; per abilitarlo:

	$ git config --global credential.helper osxkeychain

### Export a tar.gz of Preferences

To export a tar.gz of this repository run `make release`.

## Repository e utility

- [zsh-syntax-highlighting](https://github.com/zsh-users/zsh-syntax-highlighting)
- [tmux-colors-solarized](https://github.com/seebi/tmux-colors-solarized.git)
- [dircolors-solarized](https://github.com/seebi/dircolors-solarized.git)

## Note

### Vim e css.vim (OLD)

Il plugin `css.vim` rende lentissima l'apertura di file html, css, e simili in
console; vedi anche [qui](http://markhansen.co.nz/vim-slow-html/); il problema
si risolve caricando lo script solo con la GUI.

### OS X, zsh and brew completion

To enable brew completion with zsh:

```
ln -s /usr/local/Library/Contributions/brew_zsh_completion.zsh ~/.zsh/func/_brew
```
