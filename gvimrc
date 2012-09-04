" Vim - Configurazione generica
" vim: foldmarker={{{,}}} foldlevel=0 foldmethod=marker
"
"    //   \o FOTTI GRAFICAMENTE
"  -oOO __/)  IL 
"    '`  (\   FUCO

" Opzioni specifiche per Sistema Operativo / Interfaccia grafica {{{
" Linux-only (gtk2 port) {{{
if has("gui_gtk2")
    " FONTS
    " Una selezione di possibili font decenti, in ordine di decenza.
    " NOTA: Alcuni, tipo Proggy, si vedono bene solo a size 12
    " set guifont=Liberation\ Mono\ 9
    " set guifont=ProggySquareTT\ 12
    " set guifont=Terminus\ 10
    " set guifont=ProggyCleanTTSZ\ 12
    " set guifont=Bitstream\ Vera\ Sans\ Mono\ 9
    " set guifont=Monospace\ 10
    " set guifont=DejaVu\ Sans\ Mono\ 9
	set guifont=Consolas\ 9

    " COLORSCHEME
    " colorscheme habiLight
    " colorscheme sienna
	" colo mustang
	" colo martin_krischik
	colorscheme inkpot
" }}}

" X11 (Linux?) {{{
elseif has("x11")
    " Also for GTK 1
    :set guifont=-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15
" }}}

" Windows {{{
elseif has("gui_win32")
    :set guifont=Luxi_Mono:h12:cANSI
" }}}

" Mac OS X (MacVim) {{{
elseif has("gui_macvim")
    " colorscheme molokai
    " colorscheme habilight
    " colorscheme autumnleaf
    " colorscheme autumn
    " colorscheme eclipse
    " colorscheme slate
	" 'native' e' OK.
	" colorscheme native 
	" colorscheme navajo-night
	" colorscheme mustang
	" colorscheme xoria256
	colorscheme inkpot

    " I FONTI POMPI
    " set guifont=Monaco:h12
    " set guifont=Menlo:h12
     "set gfn=Osaka-Mono:h14
    " set guifont=DejaVu\ Sans\ Mono:h12
	" set guifont=Menlo:h10
	" set guifont=Inconsolata:h12
	set guifont=Menlo\ Regular:h10

    " Configurazione finestra (altezza, larghezza, trasparenza)
    set lines=55
    set columns=128
    set transp=1
" }}}

" Generic GUI (?) {{{
else
    " e' una gui ma non so di che tipo:
    colorscheme sienna
" }}}

endif
" }}}

" Opzioni generiche {{{
set guioptions-=T			" Disattiva la toolbar
set guicursor=a:blinkon0	" Cursore che non blinka
" set toolbar=icons,tooltips

set mousehide				" Hide the mouse when typing text
set mouse=a					" Usa il mouse per tutti i Mode
" set vb					" meglio la visual bell che l'orrendo SPEAKER
" set ghr=2

" Evito il seguente bug: quando si crea il primo tab con :tabc o simili, la
" statusline di vim viene ridimensionata e diventa invisibile.
set showtabline=2	 " 2 = always

" mouse clipboard
" in questo modo di default usare il buffer "ctrl-c/ctrl-v"
" NOTA: unnamedplus e' disponibile solo su X11
if has('unnamedplus')
	set clipboard=unnamedplus
else
	set clipboard=unnamed
endif

" il colore dei numeri in caso di opzione 'number'
highlight LineNr guifg=blue
" }}}
