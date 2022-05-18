" Vim - Configurazione generica
" vim: foldmarker={{{,}}} foldlevel=0 foldmethod=marker
"
"    //   \o FOTTI
"  -oOO __/)  IL
"    '`  (\   FUCO


" NOTE, SUGGERIMENTI E AVVERTIMENTI ---------------------------------------- {{{
" - lo spazio tra ogni sezione e' di due righe vuote
" - :options apre una finestra dove vedere e cambiare le opzioni
" - per l'help delle opzioni utilizzare la sintassi: help 'nome opzione'
" - OMNI-Completion: C-x, C-o
" - help comandi finestre: :he CTRL-W
" - inserire lettere accentate: CTRL-K + lettera + ' o !
"   Ad esempio per inserire á: C-k + a + '
" - apre una tag in una nuova finestra: C-w + ]
" - chiude "l'altra" finestra, come emacs: C-w C-o
"
" Colorscheme interessanti
" LIGHT: autumn
" DARK: inkpot, xoria256, badwolf, moria
"
" }}}


" Creazione delle directory necessarie (all'avvio di vim) ------------------ {{{
if !isdirectory(expand("~/.vim/backups"))
    call mkdir(expand("~/.vim/backups"), "", 0700)
endif
" }}}


" Configurazione di base --------------------------------------------------- {{{
set nocompatible		" si comporta da vim e non da vi :)
set backspace=indent,eol,start	" permette il backspace sempre
set backup			" crea una copia di backup prima di sovrascrivere
set backupdir=~/.vim/backups,.	" directory per i file di backup
set backupskip=/tmp/*,/private/tmp/*	" Fixa il problema di vim e i file crontab (anche su OSX)

" set bg=dark			" background NERO
set background=light
set nocursorcolumn		" evidenzia la colonna dove si trova il cursore, ma e' LENTO!

set encoding=utf-8		" default encoding
set history=200			" quante entry di history per comandi e search
set completeopt=longest,menuone,preview	" Completion piu' dettagliata e comoda
set noexrc				" NON leggere i file .vimrc .exrc nella dir corrente.

set ignorecase			" ricerca case insensitive...
set incsearch			" ricerca incrementale
set infercase			" ...anche nella completion
set smartcase			" ...MA se la ricerca contiene caratteri uppercase, annulla ignorecase
set wrapscan			" la ricerca di testo NON si ferma alla fine del file
set grepprg=ack\ -a		" usa ack al posto di grep per ':grep' (-a per cercare in tutti i file!)
set grepformat=%f:%l:%m " per usare ack
set path=./**,**		" i path per il comando :find, :tabfind, etc (comodo!)
set fillchars=diff:⣿,vert:\|,fold:-
"set formatoptions=rq ?		" XXX
set laststatus=2		" mostra sempre la riga di status con le info sul file
" set lazyredraw		" non fare il redraw dello schermo mentre runna le macro
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮	" I caratteri da usare in `list` mode
set showbreak=↪			" Il carattere da mostrare alla fine delle righe wrappate
set nomodeline			" NON uso le modlines, ma le securemodlines tramite plugin
" set modeline
" set modelines=5                 " numero di righe valido per le modeline
set mousemodel=popup	" tasto destro del mouse mostra un popup, come su windows.
set report=0			" Mostra sempre il numero di righe modificate da un comando   
set ruler				" mostra la posizione del cursore in basso a destra
set scrolloff=5			" scrolla con un context di 3 righe
set sidescroll=3		" scrolla lateralmente con un context di 3 righe
set showcmd				" mostra comandi parziali mentre vengono digitati
set noshowmatch			" NON mostrare la parentesi corrispettiva quando ne inserisci una
set showmode			" mostra un messaggio se in modalita' insert/visual/replace
" NOTA: Siccome la statusline include una funzione di fugitive (plugin vim) che
" puo' NON essere installato, sposto la definizione della statusline piu' in
" basso, dopo i Bundle, e ci metto un if.
set splitbelow			" Splitta aprendo sempre una finestra sotto quella attuale
set splitright			" Splitta aprendo sempre una finestra a destra di quella attuale
set title				" Permette di modificare il titolo della finestra/term
set synmaxcol=800		" Non fare l'highlight di righe piu' lunghe di 800 colonne (file XML abnormi & co)
set switchbuf=useopen,usetab " Quando switcha buffer vede prima se è aperto in un'altra finestra o tab.

set nosmartindent		" NON indentare con saggezza
set virtualedit=block	" permette di posizionare il cursore dove NON ci sono caratteri,
						" in visual block
set wildignore=*.o,*.obj,*.exe,*.jpg,*.gif,*.bmp,*.png
set wildignore+=*.py[co],*.DS_Store

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
" }}}


" Mouse support ------------------------------------------------------------ {{{
" if has('mouse')
"     set mouse=a
" endif
set mouse=""
" }}}


" Folding ------------------------------------------------------------------ {{{
set foldenable
" set foldmethod=marker
set foldlevel=100		" trick per non foldare automaticamente
" set foldcolumn=2
set foldopen=block,hor,mark,percent,quickfix,tag    " what movements open folds
" }}}


" Lingua e dizionari {{{
set spelllang=it,en,en_us
set dictionary+=/usr/share/dict/words

" thesaurus (sinonimi) italiano:
" Per usarlo: C-x C-t
" http://linguistico.sourceforge.net/pages/thesaurus_italiano.html
" http://www.thegeekstuff.com/2008/12/vi-and-vim-editor-3-steps-to-enable-thesaurus-option/
" ftp://ibiblio.org/pub/docs/books/gutenberg/etext02/mthes10.zip
" set thesaurus+=/Users/sand/Documents/thesaurus/mthesaur.txt
" }}}


" GUI {{{
" se il terminale supporta i colori, abilita sintassi colorata e ricerca
" con highlight
if &t_Co > 2 || has("gui_running")
    " impostare il modo di syncare la sintassi?
    " syntax sync fromstart
    syntax on
	" Forse ODIO hlsearch.
    " set nohlsearch
    set hlsearch
    
    "hi statusline ctermfg=Black ctermbg=Blue
endif
" }}}


" Settaggi utili quando si usa vim come IDE {{{
" Questo dovrebbe essere un "%" evoluto
runtime macros/matchit.vim

" Text justification (:Justify)
runtime macros/justify.vim

" signs
sign define information text=!> linehl=Warning texthl=Error
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

" Plugin Vim - Pathogen {{{
" https://github.com/tpope/vim-pathogen.git
" NOTA: va chiamato con `filetype off` e prima di `filetype indent on`.
" filetype off 
execute pathogen#infect()
syntax on
filetype plugin indent on
" }}}

" colori {{{
" Vanno dopo pathogen, in caso usi pluginz

if !has("gui_running")
    " colorscheme candycode
    " colorscheme fnaqevan
    " colorscheme molokai
	" colorscheme xoria256
	set bg=dark
	colorscheme gruvbox
endif

" }}}

" Statusline {{{
set statusline=%f
set stl+=\ 
set stl+=%h
set stl+=%m
set stl+=%r
set stl+=%w
set stl+=\ 
" set stl+=%{fugitive#statusline()}
" set stl+=\ 
set stl+=%#warningmsg#
"set stl+=%{SyntasticStatuslineFlag()}
set stl+=%*
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
" }}}

" Opzioni plugin & co {{{
let python_highlight_all=1		" :he ft-python-syntax; abilita l'highlight per tutto
let perl_extended_vars=1 		" highlight advanced perl vars inside strings
let perl_include_pod=1	    		" highlight POD correclty, dicono

" tolgo l'highlight degli spazi vuoti alla fine delle righe: e' fastidioso.
" NOTA: per info vedi syntax/python.vim
let python_highlight_space_errors = 0

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

" vim-json
let g:vim_json_syntax_conceal = 0
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
	"autocmd BufEnter * :syntax sync fromstart

	" Resize splits when the window is resized
	" src: https://bitbucket.org/sjl/dotfiles/src/tip/vim/.vimrc
	" au VimResized * exe "normal! \<c-w>="
	au VimResized * :wincmd =

	" 14/11/12 Disattivo lo spell check perchè causa problemi di encoding
	" e altre stronzate; non e' affatto comodo.
	" spell check per i commit di git
	" autocmd FileType gitcommit setl spell
	" e per i file Markdown
	" autocmd FileType markdown setl spell

	" autocmd FileType mail,human set formatoptions+=t textwidth=72 nosmartindent

	" Cambia colore della status line in insert mode
	augroup ft_statuslinecolor
		au!
		au InsertEnter * hi StatusLine term=bold,reverse gui=bold ctermfg=196 ctermbg=103 guifg=#ffffff guibg=#ce4e4e
		au InsertLeave * hi StatusLine term=bold,reverse gui=bold ctermfg=231 ctermbg=103 guifg=#ffffcd guibg=#306d30
	augroup END

	" Il numero di riga con il tema oceanblack sembra OK
	if has("gui_running")
		autocmd FileType perl,shell :setl number
	endif

	" ignore le modeline nei commit di git.
	autocmd BufNewFile,BufRead COMMIT_EDITMSG :let g:secure_modelines_modelines=0

	" Visto che non credo scrivero' mai in Modula2 dico a vim che '.md' e'
	" Markdown e non Modula2:
	au BufNewFile,BufRead *.md setl ft=markdown

	" When editing a file, always jump to the last known cursor position.
	" Don't do it when the position is invalid or when inside an event handler
	" (happens when dropping a file on gvim).
	autocmd BufReadPost *
	  \ if line("'\"") > 0 && line("'\"") <= line("$") |
	  \   exe "normal g`\"" |
	  \ endif

	" Utilizza la `syntax completion` se non esite una omnicompletion apposita
	" per questo tipo di file; questo trucco, suggerito dall'help di
	" `compl-omni`, va posto dopo tutti gli autocommand FileType.
	autocmd FileType * if &omnifunc == "" | setl omnifunc=syntaxcomplete#Complete | endif

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

" toggle list - \tl
nnoremap <Leader>tl :set invlist list?<CR>

" inserisce la data (imap = in modalita' insert) - \dd - \dmy
nmap <Leader>dd :.!date +"\%H:\%M -  "<CR>$
imap <Leader>dmy <C-R>=strftime("%d-%m-%y")<CR>
" inserire la data nel formato inglese: Month Day, Year (non so su cosa
" basi la localizzazione).
imap <Leader>ddd <C-R>=strftime("%B %d, %Y")<CR>

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

" Ardito, ma interessante:
" Rimappare altri tasti in vece di <ESC>
" In questo modo, in INSERT MODE, premere jj e' come premere <ESC>
" inoremap jj <ESC>

" Emacs bindings in command line mode
cnoremap <c-a> <home>
cnoremap <c-e> <end>

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
" http://www.example.com
" http://www.internet.org
" ...
" fai, partendo dall'inizio della prima riga:
" qa2f/ly$:.!host CTRL-r"<c-r>jq
"
" HAI CAPITO? CTRL-r INCOLLA NELLA COMMAND LINE! e -> " <- e' l'unnamed register
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
