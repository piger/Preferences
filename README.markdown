# Preferences.git -- i miei file di configurazione per la command line.

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

- CSApprox (supporto temi 256 colori per vecchi vim/term)
- TTCoach (il training per scrivere)

## Git

Su questo articolo di github dicono che le versioni recenti (> 1.6) di git
includono un helper per integrarsi con keychains di OS X; per abilitarlo:

	$ git config --global credential.helper osxkeychain

### Esportare Preferences

Per esportare tutto il repository in formato tar.gz:

	$ git archive --format=tar.gz --prefix=Preferences/ --output=Preferences_$(date +"%d-%b-%Y").tar.gz HEAD

## Repository e utility

- [zsh-syntax-highlighting](git://github.com/zsh-users/zsh-syntax-highlighting.git)
- [tmux-colors-solarized](https://github.com/seebi/tmux-colors-solarized.git)
- [dircolors-solarized](https://github.com/seebi/dircolors-solarized.git)
- [cliweather](https://github.com/AaronFoltz/cliweather.git) - per il clima in
  command line.
- # gem install lolcat
- [python-mode](https://github.com/klen/python-mode.git)

## Note

Il plugin `css.vim` rende lentissima l'apertura di file html, css, e simili in
console; vedi anche [qui](http://markhansen.co.nz/vim-slow-html/); il problema
si risolve caricando lo script solo con la GUI.

Per la completion di brew su OSX, linkare:

	/usr/local/Library/Contributions/brew_zsh_completion.zsh in ~/.zsh/func/_brew
