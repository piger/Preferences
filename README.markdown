# Preferences.git -- i miei file di configurazione per la command line.

Installazione submodules git:

	$ git submodule add <URL> <path>

Ad esempio:

	$ git submodule add http://github.com/sjl/gundo.vim.git ~/.vim/bundle/Gundo

Ricordarsi di fare il commit con il file `.gitmodules` aggiornato, e negli altri
repo fare:

	$ git submodule update --init

## Vim

Lista di plugin che potrebbero essere interessanti ma non mi servono e/o non
utilizzo mai:

- CSApprox (supporto temi 256 colori per vecchi vim/term)
- TTCoach (il training per scrivere)

## Repository e utility

- [zsh-syntax-highlighting](git://github.com/zsh-users/zsh-syntax-highlighting.git)
- [tmux-colors-solarized](https://github.com/seebi/tmux-colors-solarized.git)
- [dircolors-solarized](https://github.com/seebi/dircolors-solarized.git)
- [cliweather](https://github.com/AaronFoltz/cliweather.git) - per il clima in
  command line.
- # gem install lolcat

## Note

Il plugin `css.vim` rende lentissima l'apertura di file html, css, e simili in
console; vedi anche [qui](http://markhansen.co.nz/vim-slow-html/); il problema
si risolve caricando lo script solo con la GUI.
