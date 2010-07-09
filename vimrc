" vimrc [updated: 18-09-2009]
" Daniel Kertesz <daniel@spatof.org> http://spatof.org
" vim: set foldmarker={,} foldlevel=0:


" NOTE, SUGGERIMENTI E AVVERTIMENTI {
"	- lo spazio tra ogni sezione e' di due righe vuote
"	- :options apre una finestra dove vedere e cambiare le opzioni
"	- per l'help delle opzioni utilizzare la sintassi: help 'nome opzione'
" }


" Configurazione di base {
set backspace=indent,eol,start	" permette il backspace sempre
set backup			" crea una copia di backup prima di sovrascrivere
set backupdir=~/.vim/backups,.	" directory per i file di backup
" Per ogni dir in &backupdir, controlla se esiste e in caso la crea
for d in split(&backupdir, ",")
    let dd = expand(d)
    if !isdirectory(dd)
	call mkdir(dd, "p", 0700)
    endif
endfor
" --
set bg=dark			" background NERO
set nocompatible		" si comporta da vim e non da vi :)
set nocursorcolumn		" evidenzia la colonna dove si trova il cursore, ma e' LENTO!

" Directory per i file di swap (crea se necessario)
set directory=~/.vim/swap,.
if !isdirectory($HOME . "/.vim/swap")
    call mkdir($HOME . "/.vim/swap", "p", 0700)
endif

set history=50			" quante entry di history per comandi e search

" ricerca testo
set ignorecase			" ricerca case insensitive...
set infercase			" ...anche nella completion
set smartcase			" ...MA se la ricerca contiene caratteri uppercase, annulla ignorecase
set incsearch			" ricerca incrementale
" set nowrapscan		" la ricerca di testo si ferma alla fine del file, senza wrappare

"set formatoptions=rq ?		" XXX
set laststatus=2		" mostra sempre la riga di status con le info sul file
set lazyredraw			" non fare il redraw dello schermo mentre runna le macro
set listchars=tab:>-,trail:-	" In 'list', mostra ">----" per i tab e "---" per gli spazi vuoti alla fine delle righe
set nomodeline			" NON uso le modlines, ma le securemodlines tramite plugin
" set modelines=5                 " numero di righe valido per le modeline
set report=0			" Mostra sempre il numero di righe modificate da un comando   
set ruler			" mostra la posizione del cursore in basso a destra
set scrolloff=3			" scrolla con un context di 3 righe
set showcmd			" mostra comandi parziali mentre vengono digitati
set noshowmatch			" NON mostrare la parentesi corrispettiva quando ne inserisci una
set showmode			" mostra un messaggio se in modalita' insert/visual/replace
set statusline=%<%F\ %h%m%r%w%=\ [FORMAT=%{&ff}]\ %([TYPE=%Y]\ %)[POS=%04l,%04v][%p%%]\ [LEN=%L]
set nosmartindent		" NON indentare con saggezza
set t_Co=256			" 256 colori
set virtualedit=block		" permette di posizionare il cursore dove NON ci sono caratteri, in visual block
set wildignore=*.o,*.obj,*.exe,*.pyc,*.jpg,*.gif,*.bmp,*.png
set wildmenu			" Abilita il menu carino per la completion
set wildmode=list:longest,full	" Complete longest common string, then each full match
set wrap			" wrappa SEMPRE, e' OK!

" Mouse support XXX {
" Se il terminal emulator supporta il mouse, usalo... ma anche no
" if has('mouse')
"     set mouse=a
" endif
" set clipboard+=unnamed " share windows clipboard
" }

" folding {
set foldenable
set foldmarker={,}
set foldmethod=marker
set foldlevel=100		" trick per non foldare automaticamente
" set foldcolumn=2
set foldopen=block,hor,mark,percent,quickfix,tag    " what movements open folds
"function SimpleFoldText() " {
"    return getline(v:foldstart).' '
"endfunction " }
"set foldtext=SimpleFoldText()
" }

" il viminfo e' un file dove vengono salvate informazioni di sessione per
" gli ultimi file editati (es: ricerche, comandi, marks ...
" '10   =>      ricorda marks per ultimi 10 file editati
" /10   =>      ricorda le ultime 10 ricerche
" :10   =>      ricorda ultimi 10 comandi
" <20   =>      ricorda massimo 20 righe per registro
" f1    =>      ricorda marks
" h     =>      disabilita l'highlight della sessione precedente
" n~/.viminfo => nome del file
if v:version >= 700
    "set viminfo='10,/10,:10,<20,f1,h,n~/.viminfo
    set viminfo='100,f1,<200,/50,:100,h
endif
" }


" GUI e colori {
" se il terminale supporta i colori, abilita sintassi colorata e ricerca
" con highlight
if &t_Co > 2 || has("gui_running")
    " impostare il modo di syncare la sintassi?
    " syntax sync fromstart
    syntax on
    set hlsearch
    
    hi statusline ctermfg=Black ctermbg=Blue
endif

if has("gui_running")
    if has("gui_gtk2")
	" Una selezione di possibili font decenti, in ordine di decenza.
	" NOTA: Alcuni, tipo Proggy, si vedono bene solo a size 12
	" set guifont=Liberation\ Mono\ 9
	" set guifont=ProggySquareTT\ 12
	set guifont=Terminus\ 10
	colorscheme oceanblack
	" set guifont=ProggyCleanTTSZ\ 12
	" set guifont=Bitstream\ Vera\ Sans\ Mono\ 9
    elseif has("x11")
	" Also for GTK 1
	:set guifont=-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15
    elseif has("gui_win32")
	:set guifont=Luxi_Mono:h12:cANSI
    elseif has("gui_macvim")
	"colorscheme molokai
	colorscheme habilight
	" set guifont=Monaco:h12
	set guifont=DehaVu\ Sans\ Mono:h12
	set lines=37
	set columns=107
    else
	colorscheme oceanblack
    endif

    set mousehide	" Hide the mouse when typing text
    set vb		" meglio la visual bell che l'orrendo SPEAKER

    " COLOR SCHEME
    " Per utilizzare i colorscheme di gvim usa plugins/CSApprox.vim
    " E' consigliabile fargli generare il nuovo colorscheme "compatibile"
    " (vedi help) e disabilitarlo.
    
    " colorscheme non male:
    " oceandeep (su gVim), vividchalk, asu1dark, peachpuff (gui), ron
    
    "colorscheme dw_orange-256colors
    "colorscheme asu1dark-256colors
else
    " non ha GUI running
    colorscheme dante
endif
" }


" Settaggi utili quando si usa vim come IDE {
" set number
" set cursorline
" set statusline=%<%f\%h%m%r%=%-20.(line=%l\ \ col=%c%V\ \ totlin=%L%)\ \ \%h%m%r%=%-40(bytval=0x%B,%n%Y%)\%P
source $VIMRUNTIME/macros/matchit.vim
" }


" formattazione {
" :he 'tabstop'

" There are four main ways to use tabs in Vim:
" 1. Always keep 'tabstop' at 8, set 'softtabstop' and 'shiftwidth' to 4
"    (or 3 or whatever you prefer) and use 'noexpandtab'.  Then Vim
"    will use a mix of tabs and spaces, but typing <Tab> and <BS> will
"    behave like a tab appears every 4 (or 3) characters.

set tabstop=8                   " numero di spazi per <Tab>
set shiftwidth=4                " numero di spazi per 'step' di indent
set softtabstop=4
set noexpandtab
"set shiftround                  " indenta per multipli di shiftwidth
"set autoindent                  " indenta ogni riga seguendo l'indentatura della precedente
" }


" Opzioni plugin & co {
let python_highlight_all=1		" :he ft-python-syntax; abilita l'highlight per tutto
let perl_extended_vars=1 		" highlight advanced perl vars inside strings
let perl_include_pod=1	    		" highlight POD correclty, dicono

" NERDTree
let NERDTreeShowBookmarks = 1		" Mostra i bookmarks
let NERDTreeQuitOnOpen = 1		" Esci da NerdTree dopo aver aperto un file

" GetLatestVimScripts
"let g:GetLatestVimScripts_allowautoinstall=1	" XXX da verificare

" Secure Modelines
let g:secure_modelines_verbose = 1	" Avvisa quando blocca qualche modeline
let g:secure_modelines_allowed_items = [
	    \ "textwidth",   "tw",
	    \ "softtabstop", "sts",
	    \ "tabstop",     "ts",
	    \ "shiftwidth",  "sw",
	    \ "expandtab",   "et",   "noexpandtab", "noet",
	    \ "filetype",    "ft",
	    \ "foldmethod",  "fdm",
	    \ "readonly",    "ro",   "noreadonly", "noro",
	    \ "rightleft",   "rl",   "norightleft", "norl",
	    \ "spell",
	    \ "spelllang",
	    \ "foldlevel", "fdl",
	    \ "fileencoding", "fenc"
	    \ ]

" TwitVim configuration
if filereadable($HOME . "/.twitter.vim")
    source $HOME/.twitter.vim
endif

" FuzzyFinder
let g:fuf_infoFile = '~/.vim/vim-fuf'

" MiniBufExplorer
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplModSelTarget = 1

" There is a VIM bug that can cause buffers to show up without
" their highlighting. The following setting will cause MBE to
" try and turn highlighting back on (introduced in 6.3.1):
let g:miniBufExplForceSyntaxEnable = 1

" python complete
" Funziona solo se vim e' compilato con python. Lo script originale BARFA
" un orrendo messaggio di errore che va commentato.
" if !has('python')
"---->"echo "Error: Required vim compiled with +python"
"     finish
" endif

" python syntax (syntax/python.vim)
let python_slow_sync = 1
" }


" autocommands {
" Only do this part when compiled with support for autocommands.
if has("autocmd")
    " Enable file type detection.
    " Use the default filetype settings, so that mail gets 'tw' set to 72,
    " 'cindent' is on in C files, etc.
    " Also load indent files, to automatically do language-dependent indenting.
    filetype plugin indent on

    " rimuove tutti gli autocommand per evitare doppioni
    " autocmd!
    " NEW: uso questo altro metodo per vedere se ci sono altri autocmd standard
    if !exists("autocommands_loaded")
	let autocommands_loaded = 1

	" autocmd BufNewFile,BufRead *.txt set filetype=human
	" autocmd FileType mail,human set formatoptions+=t textwidth=72 nosmartindent
	
	" backup in $PWD e altro (da aggiungere)
	" autocmd BufRead /home/pentest/*	set backupdir=. nosmartindent noautoindent 
	augroup PenPen
	    au!
	    au BufNewFile,BufRead /home/pentest/* setl backupdir=.
	augroup END
	
	" For all text files set 'textwidth' to 78 characters.
	" autocmd FileType text setlocal textwidth=78

	" Esegue lo script con <Shift> + e:
	" autocmd FileType python map <buffer> <S-e> :w<CR>:!/usr/bin/env python % <CR>
	
	" sia perl che python
	""" autocmd FileType python,perl :setl foldcolumn=2
	""" autocmd FileType python :setl foldmethod=indent

	" Per python uso ftplugin/python.vim
	au BufNewFile *.py setl fileformat=unix
	au BufNewFile *.py setl encoding=utf-8
	
	" txt2tags
	au BufNewFile,BufRead *.t2t setl ft=txt2tags

	" tmux
	au BufNewFile,BufRead ~/.tmux.conf,/etc/tmux.conf setl ft=tmux

	" Il numero di riga con il tema oceanblack sembra OK
	if has("gui_running")
	    autocmd FileType perl,shell :setl number
	endif

	" ignore le modeline nei commit di git.
	autocmd BufNewFile,BufRead COMMIT_EDITMSG :let g:secure_modelines_modelines=0

	" template vuoti!
	autocmd BufNewFile *.pl 0r ~/.vim/templates/perl.pl
	autocmd BufNewFile *.py 0r ~/.vim/templates/python.py
	
	autocmd FileType perl set makeprg=perl\ -c\ %\ $*
	autocmd FileType perl set errorformat=%f:%l:%m
	" autocmd FileType perl set autowrite

	" Tags automatiche (test)
	"autocmd BufWinEnter * silent :let &tags = expand("%:p:h") . "/tags"
	
	" Views automatiche per i file .rb
	" autocmd BufWinLeave *.rb mkview
	" autocmd BufWinEnter *.rb silent loadview
	
	" vim -b : edit binary using xxd-format!
	augroup Binary
	    au!
	    au BufReadPre  *.hex let &bin=1
	    au BufReadPost *.hex if &bin | %!xxd
	    au BufReadPost *.hex set ft=xxd | endif
	    au BufWritePre *.hex if &bin | %!xxd -r
	    au BufWritePre *.hex endif
	    au BufWritePost *.hex if &bin | %!xxd
	    au BufWritePost *.hex set nomod | endif
	augroup END

	" When editing a file, always jump to the last known cursor position.
	" Don't do it when the position is invalid or when inside an event handler
	" (happens when dropping a file on gvim).
	autocmd BufReadPost *
	  \ if line("'\"") > 0 && line("'\"") <= line("$") |
	  \   exe "normal g`\"" |
	  \ endif

    endif

else

  set autoindent                " always set autoindenting on

endif " has("autocmd")
" }


" Script e funzioni {
" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
" WARNING: FA UN MACELLO !
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
	 \ | wincmd p | diffthis
endif
" }


" Abbreviazioni {
abbreviate teh the
abbreviate subent subnet
" }


" Mappings, Shortcuts & Bindings {
" WARNING: <Leaders> defaults to "\"
" F2	- toggle highlight
" F3	- toggle autoindent
" F4	- toggle paste

" Don't use Ex mode, use Q for formatting
map Q gq

" toggle highlight - \th - F4
nnoremap <Leader>th :set invhls hls?<CR>
nmap <F2> <Leader>th

" toggle autoindent - \th - F3
nnoremap <Leader>tai :set invautoindent autoindent?<CR>
nmap <F3> <Leader>tai

" inverte paste e mostra il suo valore - \tp - F4
nnoremap <Leader>tp :set invpaste paste?<CR>
nmap <F4> <Leader>tp
imap <F4> <C-O><Leader>tp
set pastetoggle=<F4>

" toggle list - \tl
nnoremap <Leader>tl :set invlist list?<CR>

" inserisce la data (imap = in modalita' insert) - \dd - \dmy
nmap <Leader>dd :.!date +"\%H:\%M -  "<CR>$
imap <Leader>dmy <C-R>=strftime("%d-%m-%y")<CR>

" rot13 fun - \rot
nmap <Leader>rot ggVGg?

" PLUGINS:
" NERDTree - \nt
nmap <Leader>nt :NERDTreeToggle<CR>

" Fuzzyfinder (from the example):
let g:FuzzyFinderOptions = { 'Base':{}, 'Buffer':{}, 'File':{}, 'Dir':{}, 'MruFile':{}, 'MruCmd':{}, 'FavFile':{}, 'Tag':{}, 'TaggedFile':{}}
let g:FuzzyFinderOptions.Base.ignore_case = 1
"let g:FuzzyFinderOptions.Base.abbrev_map  = {
"      \   '\C^VR' : [
"      \     '$VIMRUNTIME/**',
"      \     '~/.vim/**',
"      \     '$VIM/.vim/**',
"      \     '$VIM/vimfiles/**',
"      \   ],
"      \ }
let g:FuzzyFinderOptions.MruFile.max_item = 200
let g:FuzzyFinderOptions.MruCmd.max_item = 200
nnoremap <silent> <C-n>      :FuzzyFinderBuffer<CR>
nnoremap <silent> <C-m>      :FuzzyFinderFile <C-r>=expand('%:~:.')[:-1-len(expand('%:~:.:t'))]<CR><CR>
nnoremap <silent> <C-j>      :FuzzyFinderMruFile<CR>
nnoremap <silent> <C-k>      :FuzzyFinderMruCmd<CR>
nnoremap <silent> <C-p>      :FuzzyFinderDir <C-r>=expand('%:p:~')[:-1-len(expand('%:p:~:t'))]<CR><CR>
nnoremap <silent> <C-f><C-d> :FuzzyFinderDir<CR>
nnoremap <silent> <C-f><C-f> :FuzzyFinderFavFile<CR>
nnoremap <silent> <C-f><C-t> :FuzzyFinderTag!<CR>
nnoremap <silent> <C-f><C-g> :FuzzyFinderTaggedFile<CR>
noremap  <silent> g]         :FuzzyFinderTag! <C-r>=expand('<cword>')<CR><CR>
nnoremap <silent> <C-f>F     :FuzzyFinderAddFavFile<CR>
nnoremap <silent> <C-f><C-e> :FuzzyFinderEditInfo<CR>

"   * bufexplorer
"   <Leader>be  - Opens BufExplorer
"   <Leader>bs  - Opens horizontally split window BufExplorer
"   <Leader>bv  - Opens vertically split window BufExplorer
"
"   * man page viewer
"   K	- exec ManPageView
"
"   * DirDiff
"   :DirDiff /path/1 /path/2
"
"   * fuzzyfinder
"       :FuzzyFinderBuffer      - launchs buffer-mode Fuzzyfinder.
"       :FuzzyFinderFile        - launchs file-mode Fuzzyfinder.
"       :FuzzyFinderDir         - launchs directory-mode Fuzzyfinder.
"       :FuzzyFinderMruFile     - launchs MRU-file-mode Fuzzyfinder.
"       :FuzzyFinderMruCmd      - launchs MRU-command-mode Fuzzyfinder.
"       :FuzzyFinderFavFile     - launchs favorite-file-mode Fuzzyfinder.
"       :FuzzyFinderTag         - launchs tag-mode Fuzzyfinder.
"       :FuzzyFinderTaggedFile  - launchs tagged-file-mode Fuzzyfinder.
"
"   * Align (lettera maiuscola (T=, T@, T<, etc...) allineano a dx, lettera
"   minuscola (t#, ts, t:, etc...) a sx.
"   HIGHLIGHT: \t=, \t:
" if !hasmapto('<Plug>AM_T|')|map <unique> <Leader>T|		<Plug>AM_T||endif
" if !hasmapto('<Plug>AM_T#')	 |map <unique> <Leader>T#		<Plug>AM_T#|endif
" if !hasmapto('<Plug>AM_T,')	 |map <unique> <Leader>T,		<Plug>AM_T,o|endif
" if !hasmapto('<Plug>AM_Ts,') |map <unique> <Leader>Ts,		<Plug>AM_Ts,|endif
" if !hasmapto('<Plug>AM_T:')	 |map <unique> <Leader>T:		<Plug>AM_T:|endif
" if !hasmapto('<Plug>AM_T;')	 |map <unique> <Leader>T;		<Plug>AM_T;|endif
" if !hasmapto('<Plug>AM_T<')	 |map <unique> <Leader>T<		<Plug>AM_T<|endif
" if !hasmapto('<Plug>AM_T=')	 |map <unique> <Leader>T=		<Plug>AM_T=|endif
" if !hasmapto('<Plug>AM_T?')	 |map <unique> <Leader>T?		<Plug>AM_T?|endif
" if !hasmapto('<Plug>AM_T@')	 |map <unique> <Leader>T@		<Plug>AM_T@|endif
" if !hasmapto('<Plug>AM_Tab') |map <unique> <Leader>Tab		<Plug>AM_Tab|endif
" if !hasmapto('<Plug>AM_Tsp') |map <unique> <Leader>Tsp		<Plug>AM_Tsp|endif
" if !hasmapto('<Plug>AM_T~')	 |map <unique> <Leader>T~		<Plug>AM_T~|endif

" }


" Source local configuration {
if filereadable($HOME . "/.vimrc.local")
    source $HOME/.vimrc.local
endif
" }


" APPUNTI {
" ------------------------------------------------------------------------
" Other functions
" ------------------------------------------------------------------------
" :TOhtml   (plugin)
" Trasforma il file in HTML mantenendo gli stessi colori. 
"
" VOTAMAZZA:
" Supponi di avere una lista di host/URL/indirizzi e vuoi eseguire un comando
" su ognuno di essi (ad esempio, host):
" http://www.spatof.org
" http://www.lamentazioni.org
" ...
" fai, partendo dall'inizio della prima riga:
" qa2f/ly$:.!host CTRL-r"<c-r>jq
"
" HAI CAPITO? CTRL-r INCOLLA NELLA COMMAND LINE! e -> " <- e' l'unnamed register
" }


" python {
" Aggiunge al path di ricerca di vim (per i comandi gf, :find, etc) il
" sys.path di python. NOTA: "import vim" funziona solo dentro vim?
if has('python')
    python << EOF
import os
import sys
import vim
for p in sys.path:
    if os.path.isdir(p):
	vim.command(r"set path+=%s" % (p.replace(" ", r"\ ")))
    fd = open("/tmp/sucalora.log", "w")
    fd.write(dir(vim))
    fd.write("\n")
    fd.close()
EOF

endif
" }


" perlism {
" WARNING: vim must be compiled with +perl support!
"
" WARNING: never embed complex perl command in the body of a Vim function
" this will be recompiled and evaled each time for a tremendous loss of
" time.
" ------------------------------------------------------------------------

" WARNING: Skip to the end of vimrc if +perl support wasn't found.
if !has('perl')
    finish
endif

perl << EOF
    sub strip_var {
	my $var = shift;

	# skip empty lines
	return $var if ($var =~ /^\s*$/);

	my ($ret) = $var =~ /set ([^=\s]+)/;

	# again, skip empty lines
	return $var if (!defined($ret) or length($ret) < 1);
	if ($ret =~ /^no/) {
		$ret =~ s/^no//;
	}
	return $ret;
    }

    sub sort_vars {
	my ($firstline, $lastline) = @_;

	# Le variabili di Vim ovviamente NON arrivano al perl, bisogna usare
	# VIM::Eval() oppure passarle come argomenti alla funzione perl.

	# $firstline = VIM::Eval('a:firstline');
	# $lastline = VIM::Eval('a:lastline');

	@lines = $curbuf->Get($firstline .. $lastline);
	@sorted = sort { strip_var($a) cmp strip_var($b) } @lines;
	$curbuf->Set($firstline, @sorted)
    }
EOF

" x,y call SortVars()
" Esegue un sort alfabetico su un range di linee nel formato "set var=value"
" usato da Vim. Utile per fare il sort delle opzioni in vimrc :-P
" Utile anche per CAPIRE come funziona il perl dentro Vim...
function! SortVars() range
    exec "perl sort_vars " . a:firstline . ", " a:lastline
endfunction

" in :perldo il comando viene eseguito per ogni riga, mettendo la riga
" in $_ senza <EOL>
" perldo $_ = reverse($_);1
"
"
" WARNING: LA PARTE DI QUESTO FILE E' IGNORATA SE VIM E' COMPILATO
" SENZA IL SUPPORTO PERL!
" }