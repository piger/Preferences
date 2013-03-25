" Vim - Configurazione generica
" vim: foldmarker={{{,}}} foldlevel=0 foldmethod=marker
"
"    //   \o FOTTI GRAFICAMENTE
"  -oOO __/)  IL 
"    '`  (\   FUCO
"
" Colorscheme interessanti:
" habilight, sienna, mustang, martin_krischik, molokai,
" autumn eclipse, autumnleaf, slate, native, navajo-light,
" xoria256, inkpot.
"
" Font interessanti:
" Liberation Mono, ProggySquareTT, Terminus, ProggyCleanTTSZ, Bitstream Vera
" Sans Mono, Monospace, DejaVu Sans Mono, Monaco, Menlo, Inconsolata,
" Consolas, Osaka Mono.

" Opzioni specifiche per Sistema Operativo / Interfaccia grafica
" Linux-only (gtk2 port) {{{
if has("gui_gtk2")
	set guifont=Consolas\ 9
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
	colorscheme inkpot
	"set guifont=Monaco:h10
	"set guifont=Cousine:h11
	set guifont=Inconsolata:h13
	colo olive

    " Configurazione finestra (altezza, larghezza, trasparenza)
    "set lines=55
    "set columns=128
    set transp=1
" }}}

" Generic GUI (?) {{{
else
    " e' una gui ma non so di che tipo:
    colorscheme sienna
endif
" }}}

" Opzioni generiche {{{
set guioptions-=T			" Disattiva la toolbar
set guicursor=a:blinkon0	" Cursore che non blinka
set visualbell				" visual bell al posto del *BEEP*

" Evito il seguente bug: quando si crea il primo tab con :tabc o simili, la
" statusline di vim viene ridimensionata e diventa invisibile.
set showtabline=2			" 2 = always

" il colore dei numeri in caso di opzione 'number'
highlight LineNr guifg=blue
" }}}

" Mouse -------------------------------------------------------------------- {{{
set mousehide				" Hide the mouse when typing text
set mouse=a					" Usa il mouse per tutti i Mode

" Gestione clipboard: qualunque operazione di yank o paste utilizza la
" clipboard del window manager.
if has('unnamedplus')
	" Linux?
	set clipboard=unnamedplus
else
	" OS X
	set clipboard=unnamed
endif
" }}}

" Balloon Tooltips --------------------------------------------------------- {{{
set ballooneval
set balloondelay=400

" da "Hacking Vim"
" Se lo spell check e' attivo e il cursore del mouse e' sopra una parola
" sbagliata verranno mostrati in un popup i suggerimenti per la correzione.
function! SpellBalloon()
	let lines = spellsuggest(spellbadword(v:beval_text)[0], 5, 0)
	return join(lines, has("balloon_multiline") ? "\n" : " ")
endfunction

set balloonexpr=SpellBalloon()
" }}}

" Auto commands ---------------------------------------------------------- {{{
if !exists("gui_autocommands_loaded")
	let gui_autocommands_loaded = 1

	augroup ShellScript
		" Menu utili per editare shell script
		autocmd BufNewFile,BufRead *.sh runtime macros/shellmenu.vim
	augroup END
endif
" }}}
