" vimrc [updated: 05-11-2008]
" :options e' tuo amico!

" rimuove tutti gli autocommand per evitare doppioni
autocmd!
set nocompatible                " si comporta da vim e non da vi :)
set bg=dark                     " background NERO
set backspace=indent,eol,start  " permette il backspace sempre
set backup                      " crea una copia di backup prima di sovrascrivere
set backupdir=~/.vim/backups,.  	" directory per i file di backup
set directory=~/.vim/swap,.  	" directory per i file di swap
set history=50                  " quante entry di history per comandi e search
set ignorecase                  " ricerca case insensitive
set incsearch                   " ricerca incrementale
set laststatus=2				" mostra sempre una riga di status
set nomodeline                  " NON uso le modlines, ma le securemodlines tramite plugin
" set modelines=5                 " numero di righe valido per le modeline
set noexpandtab                 " usa SEMPRE veri tab
set ruler                       " mostra la posizione del cursore in basso a destra
set scrolloff=3                 " scrolla con un context di 3 righe
set showcmd                     " mostra comandi parziali mentre vengono digitati
set noshowmatch					" mostra la parentesi corrispettiva quando ne inserisci una
set showmode                    " mostra un messaggio se in modalita' insert/visual/replace
set smartcase                   " se la ricerca contiene caratteri uppercase, annulla ignorecase
set nosmartindent					" indenta con saggezza
set wrap                        " wrappa SEMPRE, e' OK!
set t_Co=256					" 256 colori

" Avvisa quando blocca qualche modeline
let g:secure_modelines_verbose = 1

" Opzioni da IDE
"set number
set statusline=%<%f\%h%m%r%=%-20.(line=%l\ \ col=%c%V\ \ totlin=%L%)\ \ \%h%m%r%=%-40(bytval=0x%B,%n%Y%)\%P
"set cul
source $VIMRUNTIME/macros/matchit.vim

" Un plugin molto carino per esportare un file (che sia syntax highlighted) in
" HTML.
source $VIMRUNTIME/plugin/tohtml.vim

" se il terminale supporta i colori, abilita sintassi colorata e ricerca
" con highlight
if &t_Co > 2 || has("gui_running")
    syntax on
    set hlsearch
	" oceandeep, vividchalk, asu1dark, peachpuff (gui)
	colorscheme tango
	hi statusline ctermfg=White ctermbg=Red
endif

" Se il terminal emulator supporta il mouse, usalo... ma anche no
" if has('mouse')
"   set mouse=a
" endif

" Don't use Ex mode, use Q for formatting
map Q gq

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
	set viminfo='10,/10,:10,<20,f1,h,n~/.viminfo
"else
"	set viminfo=%,'50,\"100,:100,n~/.viminfo
endif

" (from: http://www.stripey.com/vim/vimrc.html )
" have command-line completion <Tab> (for filenames, help topics, option names)
" first list the available options and complete the longest common part, then
" have further <Tab>s cycle through the possibilities:
set wildmode=list:longest,full


"''''''''''''''''
"	Formattazione
"""""""""""""""""
set tabstop=4                   " numero di spazi per <Tab>
set shiftwidth=4                " numero di spazi per 'step' di indent
set shiftround                  " indenta per multipli di shiftwidth
"set autoindent                  " indenta ogni riga seguendo l'indentatura della precedente

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  autocmd BufNewFile,BufRead *.txt set filetype=human
  autocmd FileType mail,human set formatoptions+=t textwidth=72 nosmartindent

  " backup in $PWD e altro (da aggiungere)
  autocmd BufRead /home/pentest/*	set backupdir=. nosmartindent noautoindent 

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " Tags automatiche (test)
  autocmd BufWinEnter * silent :let &tags = expand("%:p:h") . "/tags"

  " Views automatiche per i file .rb
  autocmd BufWinLeave *.rb mkview
  autocmd BufWinEnter *.rb silent loadview

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

  augroup END

"else
"
"  set autoindent                " always set autoindenting on

endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif

" Abbreviazioni
abbreviate teh the
abbreviate subent subnet


"''''''''''''''
"	Scorciatoie
"""""""""""""""

" <Leader> di default e' "\"

" toggle highlight
nnoremap <Leader>th :set invhls hls?<CR>
nmap <F2> <Leader>th

" toggle autoindent
nnoremap <Leader>tai :set invautoindent autoindent?<CR>
nmap <F3> <Leader>tai

" inverte paste e mostra il suo valore
nnoremap <Leader>tp :set invpaste paste?<CR>
nmap <F4> <Leader>tp
imap <F4> <C-O><Leader>tp
set pastetoggle=<F4>

" toggle list
nnoremap <Leader>tl :set invlist list?<CR>
" commentatore
nmap ,# :s/^/# /<CR>``:nohls<CR>
"nmap <F3> !as~/boxizza.pl<CR>          " <F3> boxizza la sentence sotto il cursore

" inserisce la data (imap = in modalita' insert)
nmap <Leader>dd :.!date +"\%H:\%M -  "<CR>$
imap <Leader>dmy <C-R>=strftime("%d-%m-%y")<CR>
