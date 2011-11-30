" Nessuna toolbar
set guioptions-=T
" set toolbar=icons,tooltips

set mousehide		" Hide the mouse when typing text
" set guifont=Terminus\ 10
" set vb				" meglio la visual bell che l'orrendo SPEAKER
" set ghr=2

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
	set guifont=Menlo\ Regular:h10

    " COLORSCHEME
    " colorscheme habiLight
    " colorscheme sienna
	" colo mustang
	" colo martin_krischik
	colo solarized

elseif has("x11")
    " Also for GTK 1
    :set guifont=-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15

elseif has("gui_win32")
    :set guifont=Luxi_Mono:h12:cANSI

elseif has("gui_macvim")
    " colorscheme molokai
    " colorscheme habilight
    " colorscheme autumnleaf
    colorscheme sienna
    " colorscheme eclipse
    " colorscheme slate

    " I FONTI POMPI
    " set guifont=Monaco:h12
    " set guifont=Menlo:h12
     "set gfn=Osaka-Mono:h14
    set guifont=DejaVu\ Sans\ Mono:h12

    " Configurazione finestra (altezza, larghezza, trasparenza)
    set lines=55
    set columns=128
    set transp=1

else
    " e' una gui ma non so di che tipo:
    colorscheme sienna
endif

" COLOR SCHEME PER TUTTE LE GUI (NON ATTIVO)
" Per utilizzare i colorscheme di gvim usa plugins/CSApprox.vim
" E' consigliabile fargli generare il nuovo colorscheme "compatibile"
" (vedi help) e disabilitarlo.

" colorscheme non male:
" oceandeep (su gVim), vividchalk, asu1dark, peachpuff (gui), ron

"colorscheme dw_orange-256colors
"colorscheme asu1dark-256colors

" ottobre 2011 prove
" Evito il seguente bug: quando si crea il primo tab con :tabc o simili, la
" statusline di vim viene ridimensionata e diventa invisibile.
set showtabline=2	 " 2 = always
