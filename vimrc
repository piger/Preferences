" Vim - Configurazione generica
" vim: foldmarker={{{,}}} foldlevel=0 foldmethod=marker

"    //   \o FOTTI   |
"  -oOO __/)  IL     |
"    '`  (\   FUCO   |


" NOTE, SUGGERIMENTI E AVVERTIMENTI {{{
"   - lo spazio tra ogni sezione e' di due righe vuote
"   - :options apre una finestra dove vedere e cambiare le opzioni
"   - per l'help delle opzioni utilizzare la sintassi: help 'nome opzione'
"   - OMNI-Completion: C-x, C-o
"   - help comandi finestre: :he CTRL-W
"
" IDEE, TODO
" - tasto per toggle di 'relativenumber', comodo per i comandi su piu' righe
"
" Colorscheme interessanti
" LIGHT:
" - autumn
"
" }}}


" Creazione delle directory necessarie (all'avvio di vim) {{{
if !isdirectory(expand("~/.vim/backups"))
    call mkdir(expand("~/.vim/backups"), "", 0700)
endif
if !isdirectory(expand("~/.vim/swap"))
    call mkdir(expand("~/.vim/swap"), "", 0700)
endif
" }}}


" Configurazione di base {{{
set nocompatible		" si comporta da vim e non da vi :)
set backspace=indent,eol,start	" permette il backspace sempre
set backup			" crea una copia di backup prima di sovrascrivere
set backupdir=~/.vim/backups,.	" directory per i file di backup

" set bg=dark			" background NERO
set background=light
set nocursorcolumn		" evidenzia la colonna dove si trova il cursore, ma e' LENTO!

" Directory per i file di swap ("." indica la stessa directory del file editato)
" 5-Mar-2011 - I file di swap possono anche essere in ".", non vedo motivo per
" tenerli tutti in una directory, ed e' comodo per evitare di editare in due
" lo stesso file.
" set directory=~/.vim/swap,.

" default encoding
set encoding=utf-8

set history=100			" quante entry di history per comandi e search

set noexrc			" NON leggere i file .vimrc .exrc nella dir corrente.
" ricerca testo
set ignorecase			" ricerca case insensitive...
set incsearch			" ricerca incrementale
set infercase			" ...anche nella completion
set smartcase			" ...MA se la ricerca contiene caratteri uppercase, annulla ignorecase
" set wrapscan			" la ricerca di testo NON si ferma alla fine del file, senza wrappare
set grepprg=~/bin/ack	" usa ack al posto di grep per ':grep'
set path=./**,**		" i path per il comando :find, :tabfind, etc (comodo!)

"set formatoptions=rq ?		" XXX
set laststatus=2		" mostra sempre la riga di status con le info sul file
" set lazyredraw			" non fare il redraw dello schermo mentre runna le macro
set listchars=tab:>-,trail:-	" In 'list', mostra ">----" per i tab e "---" per gli
				" spazi vuoti alla fine delle righe.
set nomodeline			" NON uso le modlines, ma le securemodlines tramite plugin
" set modeline
" set modelines=5                 " numero di righe valido per le modeline
set mousemodel=popup	" tasto destro del mouse mostra un popup, come su windows.
set report=0			" Mostra sempre il numero di righe modificate da un comando   
set ruler			" mostra la posizione del cursore in basso a destra
set scrolloff=5			" scrolla con un context di 3 righe
set showcmd			" mostra comandi parziali mentre vengono digitati
set noshowmatch			" NON mostrare la parentesi corrispettiva quando ne inserisci una
set showmode			" mostra un messaggio se in modalita' insert/visual/replace
"set statusline=%<%F\ %h%m%r%w\ %{fugitive#statusline()}\ %{VimBuddy()}%=\ [FORMAT:%{&ff}]\ %([TYPE:%Y]\ %)line:%l/%L\ col:%v\ [%p%%]
"set statusline=%<%F\ %h%m%r%w\ %{fugitive#statusline()}%=\ [FORMAT:%{&ff}]\ %([TYPE:%Y]\ %)line:%l/%L\ col:%v\ [%p%%]
"" set statusline=%<%f\ %h%m%r%w\ %{fugitive#statusline()}%=\ buf:%n\ %y\ line:%l/%L\ –\ col:%v\ [%p%%]
set statusline=%f
set stl+=\ 
set stl+=%h
set stl+=%m
set stl+=%r
set stl+=%w
set stl+=\ 
set stl+=%{fugitive#statusline()}
set stl+=%=		" right align
set stl+=\ 

" File format, encoding and type.  Ex: "(unix/utf-8/python)"
set statusline+=(
set statusline+=%n
set statusline+=\ 
set statusline+=%{&ff}                        " Format (unix/DOS).
set statusline+=\ 
set statusline+=%{strlen(&fenc)?&fenc:&enc}   " Encoding (utf-8).
set statusline+=\ 
set statusline+=%{&ft}                        " Type (python).
set statusline+=)

" Line and column position and counts.
set statusline+=\ (line\ %l\/%L,\ col\ %03c)


set nosmartindent		" NON indentare con saggezza
set virtualedit=block		" permette di posizionare il cursore dove NON ci sono caratteri,
				" in visual block
set wildignore=*.o,*.obj,*.exe,*.pyc,*.jpg,*.gif,*.bmp,*.png
set wig+=*.DS_Store

set wildmenu			" Abilita il menu carino per la completion
set wildmode=list:longest,full	" Complete longest common string, then each full match
set wrap			" wrappa SEMPRE, e' OK!
set t_Co=256			" 256 colori in terminale	
set vb				" meglio la visual bell che l'orrendo SPEAKER

" Ricorda nel viminfo:
" '100 -- marks per gli ultimi 100 file
" <50 -- massimo 20 righe per registro
" s5 -- massimo 5Kb per registro
" /20 -- ultime 20 ricerche
" :20 -- ultimi 20 comandi
set viminfo='100,<50,s5,/20,:20

" Alla TextMate, ma non mi piace.
" set list
" set listchars=tab:▸\ ,eol:¬

" new in vim 7.3: blowfish encryption [NOTA: per cryptare bisogna settare
" 'key', con il comando :X ]
if v:version >= 703
    set cryptmethod=blowfish
endif
" }}}


" Mouse support {{{
" Se il terminal emulator supporta il mouse, usalo... ma anche no
" if has('mouse')
"     set mouse=a
" endif
set mouse=""

" Questo POTREBBE essere interessante...
" set clipboard+=unnamed " share windows clipboard
" }}}


" folding {{{
"set foldenable
"set foldmethod=marker
"set foldlevel=100		" trick per non foldare automaticamente
" set foldcolumn=2
set foldopen=block,hor,mark,percent,quickfix,tag    " what movements open folds
"function SimpleFoldText() " {
"    return getline(v:foldstart).' '
"endfunction " }
"set foldtext=SimpleFoldText()
" }}}


" Lingua e dizionari {{{
set spelllang=it,en,en_US
set dictionary+=/usr/share/dict/words

" thesaurus (sinonimi) italiano:
" Per usarlo: C-x C-t
" http://linguistico.sourceforge.net/pages/thesaurus_italiano.html
" http://www.thegeekstuff.com/2008/12/vi-and-vim-editor-3-steps-to-enable-thesaurus-option/
" ftp://ibiblio.org/pub/docs/books/gutenberg/etext02/mthes10.zip
" set thesaurus+=/Users/sand/Documents/thesaurus/mthesaur.txt
" }}}


" GUI e colori {{{
" se il terminale supporta i colori, abilita sintassi colorata e ricerca
" con highlight
if &t_Co > 2 || has("gui_running")
    " impostare il modo di syncare la sintassi?
    " syntax sync fromstart
    syntax on
	" Forse ODIO hlsearch.
    set nohlsearch
    
    "hi statusline ctermfg=Black ctermbg=Blue
endif


" Colori e opzioni solo per il terminale 
if !has("gui_running")
    " colorscheme candycode
    " colorscheme fnaqevan
    " colorscheme molokai
	colorscheme native
endif
" }}}


" Settaggi utili quando si usa vim come IDE {{{
" set number
" set cursorline
" set statusline=%<%f\%h%m%r%=%-20.(line=%l\ \ col=%c%V\ \ totlin=%L%)\ \ \%h%m%r%=%-40(bytval=0x%B,%n%Y%)\%P

" Questo dovrebbe essere un "%" evoluto
runtime macros/matchit.vim
" }}}


" formattazione {{{
" :he 'tabstop'

" There are four main ways to use tabs in Vim:
" 1. Always keep 'tabstop' at 8, set 'softtabstop' and 'shiftwidth' to 4
"    (or 3 or whatever you prefer) and use 'noexpandtab'.  Then Vim
"    will use a mix of tabs and spaces, but typing <Tab> and <BS> will
"    behave like a tab appears every 4 (or 3) characters.

" 6-Mar-2011 - Cambio 'tabstop' a 4, perche' ha piu' senso che anche i tab
" altrui occupino 4 caratteri, come quelli che inserisco io (vedi shiftwidth e
" softtabstop).
" http://vimcasts.org/episodes/tabs-and-spaces/
set tabstop=4                   " numero di spazi per <Tab>
set shiftwidth=4                " numero di spazi per 'step' di indent
set softtabstop=4
set noexpandtab
"set shiftround                  " indenta per multipli di shiftwidth
"set autoindent                  " indenta ogni riga seguendo l'indentatura della precedente
" }}}


" Plugins, con Bundle (prima si usava Pathogen). {{{
" NOTA: va chiamato con `filetype off` e prima di `filetype indent on`.
filetype off 

" Pathogen e' inutile se si utilizza Vundle
" git clone http://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
"" call pathogen#helptags()
"" call pathogen#runtime_append_all_bundles()

" vundle
if isdirectory(expand("~/.vim/bundle/vundle"))
	set rtp+=~/.vim/bundle/vundle
	call vundle#rc()

	Bundle 'gmarik/vundle'

	" custom vundles
	Bundle 'Solarized'
	Bundle 'fugitive.vim'
	Bundle 'Gundo'
	" Bundle 'TTCoach'
	" Bundle 'Conque-Shell'
	Bundle 'git://github.com/acx0/Conque-Shell.git'
	Bundle 'L9'
	Bundle 'FuzzyFinder'
	" Bundle 'CSApprox'
	Bundle 'The-NERD-tree'
	Bundle 'TwitVim'
	Bundle 'bufexplorer.zip'

	" Python
	Bundle 'pydoc.vim'
	Bundle 'pyflakes.vim'
	" Bundle 'pyflakes' o 'pyflakes.vim' ??? XXX
	Bundle 'Jinja'

	" Web
	Bundle 'JSON.vim'

	" TeX
	" Bundle 'LaTeX-Suite-aka-Vim-LaTeX'
endif
" }}}


" Opzioni plugin & co {{{
let python_highlight_all=1		" :he ft-python-syntax; abilita l'highlight per tutto
let perl_extended_vars=1 		" highlight advanced perl vars inside strings
let perl_include_pod=1	    		" highlight POD correclty, dicono

" tolgo l'highlight degli spazi vuoti alla fine delle righe: e' fastidioso.
" NOTA: per info vedi syntax/python.vim
let python_highlight_space_errors = 0

" CSApprox
let g:CSApprox_verbose_level = 0

" NERDTree
let NERDTreeShowBookmarks = 1		" Mostra i bookmarks
let NERDTreeQuitOnOpen = 1		" Esci da NerdTree dopo aver aperto un file

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

" Twitvim
" NOTA: TwitVim ora usa OAuth, percio' non serve username o password qui.
" HTTP proxy
" let twitvim_proxy = "proxyserver:proxyport"
" let twitvim_proxy_login = "proxyuser:proxypassword"
" 'open' o 'gnome-open'
let twitvim_browser_cmd = 'open'
" SSL
let twitvim_api_root = "https://api.twitter.com/1"
let twitvim_filter_enable = 1
let twitvim_filter_regex = '@GetGlue\|/youtu\.be/'
let twitvim_count = 200

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

" FuzzyFinder
let g:fuf_ignoreCase = 1
let g:fuf_modesDisable = []
let g:fuf_mrufile_maxItem = 300
let g:fuf_mrucmd_maxItem = 400

" python complete
" VALUTARE pydiction:
" http://www.vim.org/scripts/script.php?script_id=850
"
" INFO:
" http://dancingpenguinsoflight.com/2009/02/python-and-vim-make-your-own-ide/
"
" Funziona solo se vim e' compilato con python. Lo script originale BARFA
" un orrendo messaggio di errore che va commentato.
" if !has('python')
"---->"echo "Error: Required vim compiled with +python"
"     finish
" endif

" python syntax (syntax/python.vim)
let python_slow_sync = 1

" Pydiction
let g:pydiction_location = '~/.vim/bundle/Pydiction/complete-dict'
" }}}


" autocommands {{{
"
" Enable file type detection.
" Use the default filetype settings, so that mail gets 'tw' set to 72,
" 'cindent' is on in C files, etc.
" Also load indent files, to automatically do language-dependent indenting.
filetype plugin indent on

" rimuove tutti gli autocommand per evitare doppioni
" autocmd!
" NEW: uso questo metodo (dall'help di autocmd) per evitare autocmd duplicati
if !exists("autocommands_loaded")
	let autocommands_loaded = 1

	" My PC is fast enough, do syntax highlight syncing from start
	autocmd BufEnter * :syntax sync fromstart

	" Resize splits when the window is resized
	" src: https://bitbucket.org/sjl/dotfiles/src/tip/vim/.vimrc
	au VimResized * exe "normal! \<c-w>="

	" autocmd BufNewFile,BufRead *.txt set filetype=human
	" autocmd FileType mail,human set formatoptions+=t textwidth=72 nosmartindent

	" backup in $PWD e altro (da aggiungere)
	" autocmd BufRead /home/pentest/*	set backupdir=. nosmartindent noautoindent 
	augroup PenPen
		au!
		" backup nella directory corrente
		au BufNewFile,BufRead */pentest/* setl backupdir=. paste
	augroup END

	" I file in ~/Documents/appunti sono tutti in formato Markdown
	augroup Appunti
		au!
		au BufNewFile,BufRead ~/Documents/appunti/* setl ft=markdown tw=80
	augroup END

	" Cambia colore della status line in insert mode
	augroup ft_statuslinecolor
		au!
		au InsertEnter * hi StatusLine term=bold,reverse gui=bold ctermfg=196 ctermbg=103 guifg=yellow guibg=#8090a0
		au InsertLeave * hi StatusLine term=bold,reverse gui=bold ctermfg=231 ctermbg=103 guifg=white guibg=#8090a0
	augroup END

	" For all text files set 'textwidth' to 78 characters.
	" autocmd FileType text setlocal textwidth=78

	" Esegue lo script con <Shift> + e:
	" autocmd FileType python map <buffer> <S-e> :w<CR>:!/usr/bin/env python % <CR>

	" sia perl che python
	""" autocmd FileType python,perl :setl foldcolumn=2
	""" autocmd FileType python :setl foldmethod=indent

	" da mitshuiko
	autocmd FileType python setlocal cinwords=in,elif,else,for,while,try,except,finally,def,class,with

	" Per python uso ftplugin/python.vim
	au BufNewFile *.py setl fileformat=unix
	au BufNewFile *.py setl encoding=utf-8

	" Per i file di Ren'Py
	au BufNewFile,BufRead *.rpy setl ft=renpy

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

	" Mako templates
	autocmd BufNewFile,BufRead *.mako setl ft=mako

	" Nmap plaintext report
	autocmd BufRead *.nmap setl ft=nmap

	" Tags automatiche (test)
	"autocmd BufWinEnter * silent :let &tags = expand("%:p:h") . "/tags"

	" Views automatiche per i file .rb
	" autocmd BufWinLeave *.rb mkview
	" autocmd BufWinEnter *.rb silent loadview

	" Per i file HTML di monbox.
	au BufNewFile,BufRead *monbox*/*.html set ft=htmldjango
	au BufNewFile,BufRead *MonitoringBox*/*.html set ft=htmldjango

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

	" file .json
	au! BufRead,BufNewFile *.json set filetype=json

	augroup json_autocmd
		autocmd!
		autocmd FileType json setl autoindent formatoptions=tcq2l
		autocmd FileType json setl textwidth=78 shiftwidth=2
		autocmd FileType json setl softtabstop=2 tabstop=8
		autocmd FileType json setl expandtab foldmethod=syntax
	augroup END

	" When editing a file, always jump to the last known cursor position.
	" Don't do it when the position is invalid or when inside an event handler
	" (happens when dropping a file on gvim).
	autocmd BufReadPost *
	  \ if line("'\"") > 0 && line("'\"") <= line("$") |
	  \   exe "normal g`\"" |
	  \ endif

	" Usa il metodo migliore di omnicompletion
	" http://vim.runpaint.org/typing/auto-completing-text/
	if exists("+omnifunc")
		au FileType *
			\ if &omnifunc == "" |
			\ setl omnifunc=syntaxcomplete#Complete |
			\ endif
	endif

	endif

" }}}


" Script e funzioni {{{
" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
	 \ | wincmd p | diffthis
endif

" QuickFixDo
" Esegue un comando su tutti i buffer 'quickfix':
"	- open all files mentioned in the QuickFix list by the following command:
" :Qfixdo tab sp
"	- In addition, it is possible to repeat the substitution itself the same
"	way.
" :Qfixdo %s/pattern/string/
" http://stackoverflow.com/questions/1830839/how-to-open-multiple-files-in-vim-after-vimgrep
command! -nargs=+ Qfixdo call QuickFixDo(<q-args>)
function! QuickFixDo(cmd)
	" let bufnam = {}
	" for q in getqflist()
	" 	" let bufnam[q.bufnr] = bufname(q.bufnr)
	" 	" 1) Usa il dict per salvare la RIGA del quickfix; l'implementazione
	" 	" originale usa un dict senza motivo, io almeno gli do un senso.
	" 	let bufnam[q.bufnr] = q.lnum
	" endfor
	" for n in keys(bufnam)
	" 	exe 'buffer' n
	" 	" 2) Salto alla riga del quickfix, altrimenti e' TUTTO INUTILE.
	" 	exe ':' + bufnam[n]
	" 	exe a:cmd
	" 	update
	" endfor

	for q in getqflist()
		exe 'buffer' q.bufnr
		" XXX assurdamente (non conosco lo scripting di Vim) posso saltare
		" alla riga giusta ma non alla colonna giusta: se tolgo i commenti e
		" chiamo tipo `:Qfixdo tab sp` il cursore me lo ritrovo all'inizio di
		" ogni documento e non alla posizione giusta :(
		exe ':' + q.lnum
		" exe q.vcol + '|'
		" exe cursor(q.lnum, q.vcol)
		exe a:cmd
	endfor
endfunction
" }}}


" Abbreviazioni {{{
abbreviate teh the
abbreviate subent subnet
abbreviate directort directory


iabbrev sl@ steve@stevelosh.com
iabbrev vrcf `~/.vimrc` file

" Copyright, Trademark, etc...
iabbrev (c) ©
iabbrev (r) ®
iabbrev (tm) ™
iabbrev (euro) €
" }}}


" Match e affini {{{
" Highlight VCS conflict markers
" src: https://bitbucket.org/sjl/dotfiles/src/tip/vim/.vimrc
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'
" }}}


" Mappings, Shortcuts & Bindings {{{
" WARNING: <Leaders> defaults to "\"
" F2	- toggle highlight
" F3	- toggle autoindent
" F4	- toggle paste

" Don't use Ex mode, use Q for formatting
map Q gq

" toggle highlight - \th - F4
nnoremap <Leader>th :set invhls hls?<CR>
nmap <F2> <Leader>th
" toglie l'highlight con \<spazio>, piu' comodo.
nnoremap <Leader><space> :noh<cr>

" toggle autoindent - \th - F3
nnoremap <Leader>tai :set invautoindent autoindent?<CR>
nmap <F3> <Leader>tai

" inverte paste e mostra il suo valore - \tp - F4
nnoremap <Leader>tp :set invpaste paste?<CR>
nmap <F4> <Leader>tp
imap <F4> <C-O><Leader>tp
set pastetoggle=<F4>

" GUNDO
nnoremap <F5> :GundoToggle<CR>

" toggle list - \tl
nnoremap <Leader>tl :set invlist list?<CR>

" inserisce la data (imap = in modalita' insert) - \dd - \dmy
nmap <Leader>dd :.!date +"\%H:\%M -  "<CR>$
imap <Leader>dmy <C-R>=strftime("%d-%m-%y")<CR>

" rot13 fun - \rot
nmap <Leader>rot ggVGg?

" salva con sudo
" http://stackoverflow.com/questions/95072/what-are-your-favorite-vim-tricks/96492#96492
cmap w!! %!sudo tee > /dev/null %


" Tab Navigation (ala Firefox): ctrl-tab, ctrl-shift-tab, ctrl-t
nmap <C-tab> :tabnext<CR>
nmap <C-S-tab> :tabprevious<CR>
map <C-tab> :tabnext<CR>
map <C-S-tab> :tabprevious<CR>
imap <C-tab> <ESC>:tabnext<CR>
imap <C-S-tab> <ESC>:tabprevious<CR>
" tolgo il bind da CTRL-t che serve a saltellare con le tag.
"nmap <C-t> :tabnew<CR>
"imap <C-t> <ESC>:tabnew<CR>

" Questo e' IPER. SUPERIORE.
" da: http://stevelosh.com/blog/2010/09/coming-home-to-vim/
" The first two lines fix Vim’s horribly broken default regex “handling” by
" automatically inserting a \v before any string you search for. This turns
" off Vim’s crazy default regex characters and makes searches use normal
" regexes. I already know Perl/Python compatible regex formatting, why would I
" want to learn another scheme?
"
" PRIMA: \w\+
" DOPO: \w+
" OVVERO: niente escape MORTALI per ogni cosa!
nnoremap / /\v
vnoremap / /\v

" Ardito, ma interessante:
" Rimappare altri tasti in vece di <ESC>
" In questo modo, in INSERT MODE, premere jj e' come premere <ESC>
inoremap jj <ESC>


" PLUGINS:
" NERDTree - \nt
nmap <Leader>nt :NERDTreeToggle<CR>

" Fuzzyfinder (bindings presi dall'esempio nel man):
" You can open a selected item in various ways:
"         <CR>  (|g:fuf_keyOpen|)        - opens in a previous window.
"         <C-j> (|g:fuf_keyOpenSplit|)   - opens in a split window.
"         <C-k> (|g:fuf_keyOpenVsplit|)  - opens in a vertical-split window.
"         <C-l> (|g:fuf_keyOpenTabpage|) - opens in a new tab page.
" Browsa i buffer
nnoremap <silent> <C-n>			:FufBuffer<CR>
amenu Fuf.Buffer\ <C-n>			:FufBuffer<CR>

" browsa i file nella dir del buffer attuale (utile!)
nnoremap <silent> <C-p> :FufFileWithCurrentBufferDir<CR>
amenu Fuf.FileWithCurrentBufferDir\ <C-p> :FufFileWithCurrentBufferDir<CR>

" browsa i file nella dir del buffer attuale con full path
nnoremap <silent> <C-f><C-p>	:FufFileWithFullCwd<CR>
amenu Fuf.FileWithFullCwd\ <C-f><C-p> :FufFileWithFullCwd<CR>

" browsa i file nella dir attuale
nnoremap <silent> <C-f>p		:FufFile<CR>
amenu Fuf.File\ <C-f>p			:FufFile<CR>

" cambia dir dalla dir del buffer attuale
nnoremap <silent> <C-f><C-d>	:FufDirWithCurrentBufferDir<CR>
amenu Fuf.DirWithCurrentBufferDir\ <C-f><C-d> :FufDirWithCurrentBufferDir<CR>

" cambia dir con full path
nnoremap <silent> <C-f>d		:FufDirWithFullCwd<CR>
amenu Fuf.DirWithFullCwd\ <C-f>d :FufDirWithFullCwd<CR>

nnoremap <silent> <C-f>D		:FufDir<CR>
amenu Fuf.Dir\ <C-f>D			:FufDir<CR>

" browsa i file usati di recente (utile!)
nnoremap <silent> <C-j>			:FufMruFile<CR>
amenu Fuf.MruFile\ <C-j>		:FufMruFile<CR>

" browsa gli ultimi comandi dati (utile!)
nnoremap <silent> <C-k>			:FufMruCmd<CR>

" ?
nnoremap <silent> <C-b>			:FufBookmark<CR>

nnoremap <silent> <C-f><C-t>	:FufTag<CR>

nnoremap <silent> <C-f>t		:FufTag!<CR>
noremap  <silent> g]			:FufTagWithCursorWord!<CR>
nnoremap <silent> <C-f><C-f> 	:FufTaggedFile<CR>
nnoremap <silent> <C-f><C-j> 	:FufJumpList<CR>
nnoremap <silent> <C-f><C-g> 	:FufChangeList<CR>
nnoremap <silent> <C-f><C-q> 	:FufQuickfix<CR>
nnoremap <silent> <C-f><C-l> 	:FufLine<CR>
nnoremap <silent> <C-f><C-h> 	:FufHelp<CR>
nnoremap <silent> <C-f><C-b> 	:FufAddBookmark<CR>
vnoremap <silent> <C-f><C-b> 	:FufAddBookmarkAsSelectedText<CR>
" nnoremap <silent> <C-f><C-e>	:FufEditInfo<CR>

" refresha la cache di file e dir (utile!)
nnoremap <silent> <C-f><C-r>	:FufRenewCache<CR>

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
"   * Align (lettera maiuscola (T=, T@, T<, etc...) allineano a dx, lettera
"   minuscola (t#, ts, t:, etc...) a sx.
"   HIGHLIGHT: \t=, \t:

" taglist (il plugin)
map <Leader>P :TlistToggle<CR>
" Displaying tags for only one file~
" The taglist window displays the tags for all the files in the Vim buffer list
" and all the manually added files. To display the tags for only the current
" active buffer, set the 'Tlist_Show_One_File' variable to 1.
let Tlist_Show_One_File = 1

" }}}


" Source local configuration {{{
if filereadable(expand("~/.vimrc.local"))
    source ~/.vimrc.local
endif
" }}}


" APPUNTI {{{
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
" }}}


" python {{{
" DOCUMENTAZIONE: :help if_pyth.txt
" Aggiunge al path di ricerca di vim (per i comandi gf, :find, etc) il
" sys.path di python.
" 13-Mar-2011 - Forse questa cosa rallenta tutto/tanto ?
" 30/11/2011 - piu' che altro non l'ho mai usato.
"" if has('python')
""     python << EOF
"" import os
"" import sys
"" import vim
"" for p in sys.path:
"" 	if os.path.isdir(p):
"" 		vim.command(r"set path+=%s" % (p.replace(" ", r"\ ")))
"" EOF
"" 
"" endif
""" " }}}


" perlism {{{
" WARNING: vim must be compiled with +perl support!
"
" WARNING: never embed complex perl command in the body of a Vim function
" this will be recompiled and evaled each time for a tremendous loss of
" time.
" ------------------------------------------------------------------------

if has('perl')
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
endif

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
" }}}


" 'secure' {{{
" When on, ":autocmd", shell and write commands are not allowed in
" ".vimrc" and ".exrc" in the current directory and map commands are
" displayed.  Switch it off only if you know that you will not run into
" problems, or when the 'exrc' option is off.  On Unix this option is only
" used if the ".vimrc" or ".exrc" is not owned by you.  This can be
" dangerous if the systems allows users to do a "chown".  You better set
" 'secure' at the end of your ~/.vimrc then.
set secure
" }}}
