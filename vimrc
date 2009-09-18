" vimrc [updated: 14-06-2009]
" - :options e' tuo amico!
" - per cercare help sulle opzioni, racchiudre il nome tra apici, :help 'nomeopzioni'

" rimuove tutti gli autocommand per evitare doppioni
autocmd!
set backspace=indent,eol,start	" permette il backspace sempre
" --{ backups }
set backup			" crea una copia di backup prima di sovrascrivere
set backupdir=~/.vim/backups,.	" directory per i file di backup
" Crea la dir di backup se non esiste; controlla ogni directory impostata
" nella variabile &backupdir
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

set directory=~/.vim/swap,.	" directory per i file di swap
if !isdirectory($HOME . "/.vim/swap")
    call mkdir($HOME . "/.vim/swap", "p", 0700)
endif

set noexpandtab			" usa SEMPRE veri tab
set history=50			" quante entry di history per comandi e search

" --{ search }
set ignorecase			" ricerca case insensitive...
set smartcase			" ...MA se la ricerca contiene caratteri uppercase, annulla ignorecase
set incsearch			" ricerca incrementale
" set nowrapscan		" la ricerca di testo si ferma alla fine del file, senza wrappare
" --

set laststatus=2		" mostra sempre la riga di status con le info sul file
set listchars=tab:>-,trail:-	" In 'list', mostra ">----" per i tab e "---" per gli spazi vuoti alla fine delle righe
set nomodeline			" NON uso le modlines, ma le securemodlines tramite plugin
" set modelines=5                 " numero di righe valido per le modeline
set report=0			" Mostra sempre il numero di righe modificate da un comando   
set ruler			" mostra la posizione del cursore in basso a destra
set scrolloff=3			" scrolla con un context di 3 righe
set showcmd			" mostra comandi parziali mentre vengono digitati
set noshowmatch			" NON mostrare la parentesi corrispettiva quando ne inserisci una
set showmode			" mostra un messaggio se in modalita' insert/visual/replace
set nosmartindent		" NON indentare con saggezza
set t_Co=256			" 256 colori
set virtualedit=block		" permette di posizionare il cursore dove NON ci sono caratteri, in visual block
set wildignore=*.o,*.obj,*.exe,*.pyc,*.jpg,*.gif,*.bmp,*.png
set wildmenu			" Abilita il menu carino per la completion
set wildmode=list:longest,full	" Complete longest common string, then each full match
set wrap			" wrappa SEMPRE, e' OK!

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

" se il terminale supporta i colori, abilita sintassi colorata e ricerca
" con highlight
if &t_Co > 2 || has("gui_running")
    " impostare il modo di syncare la sintassi?
    " syntax sync fromstart
    syntax on
    set hlsearch
    
    " COLOR SCHEME
    " Per utilizzare i colorscheme di gvim usa plugins/CSApprox.vim
    " E' consigliabile fargli generare il nuovo colorscheme "compatibile"
    " (vedi help) e disabilitarlo.
    
    " colorscheme non male:
    " oceandeep, vividchalk, asu1dark, peachpuff (gui), ron
    
    "colorscheme dw_orange-256colors
    "colorscheme asu1dark-256colors
    colorscheme asu1dark
    
    hi statusline ctermfg=Black ctermbg=Blue
endif

if has("gui_running")
    if has("gui_gtk2")
	" Una selezione di possibili font decenti, in ordine di decenza.
	" NOTA: Alcuni, tipo Proggy, si vedono bene solo a size 12
	" set guifont=Liberation\ Mono\ 9
	set guifont=ProggySquareTT\ 12
	" set guifont=Terminus\ 10
	" set guifont=ProggyCleanTTSZ\ 12
	" set guifont=Bitstream\ Vera\ Sans\ Mono\ 9
    elseif has("x11")
	" Also for GTK 1
	:set guifont=-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15
    elseif has("gui_win32")
	:set guifont=Luxi_Mono:h12:cANSI
    endif

    set mousehide	" Hide the mouse when typing text
    set vb		" meglio la visual bell che l'orrendo SPEAKER
endif

"
" --{ mouse support }
" APPROFONDIRE QUESTA SEZIONE (XXX)
" Se il terminal emulator supporta il mouse, usalo... ma anche no
if has('mouse')
    set mouse=a
endif
" set clipboard=???

" --{ folding }
"set foldcolumn=2

" IDE stuff
"set number
" set cursorline
"set statusline=%<%f\%h%m%r%=%-20.(line=%l\ \ col=%c%V\ \ totlin=%L%)\ \ \%h%m%r%=%-40(bytval=0x%B,%n%Y%)\%P
set statusline=%<%F\ %h%m%r%w%=\ [FORMAT=%{&ff}]\ %([TYPE=%Y]\ %)[POS=%04l,%04v][%p%%]\ [LEN=%L]
source $VIMRUNTIME/macros/matchit.vim


" ------------------------------------------------------------------------ 
" Formattazione
" ------------------------------------------------------------------------ 
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
" WARNING: Faccio una prova con questi valori.
"set softtabstop=4
"set noexpandtab



" ------------------------------------------------------------------------ 
" Opzioni plugin & co
" ------------------------------------------------------------------------ 
let g:secure_modelines_verbose = 1	" Avvisa quando blocca qualche modeline
let NERDTreeShowBookmarks = 1		" Mostra i bookmarks
let NERDTreeQuitOnOpen = 1		" Esci da NerdTree dopo aver aperto un file



" ------------------------------------------------------------------------
" autocmd's
" ------------------------------------------------------------------------
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
    
    " autocmd BufNewFile,BufRead *.txt set filetype=human
    " autocmd FileType mail,human set formatoptions+=t textwidth=72 nosmartindent
    
    " backup in $PWD e altro (da aggiungere)
    " autocmd BufRead /home/pentest/*	set backupdir=. nosmartindent noautoindent 
    
    " For all text files set 'textwidth' to 78 characters.
    " autocmd FileType text setlocal textwidth=78
    autocmd FileType python :setl ts=8 sw=4 sts=4 noet tw=80 smarttab smartindent

    "autocmd FileType python
    "\ setlocal tabstop=4
    "\ setlocal softtabstop=4
    "\ setlocal shiftwidth=4
    "\ setlocal textwidth=80
    "\ setlocal smarttab
    "\ setlocal expandtab
    "\ setlocal smartindent

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

    augroup END

"else
"
"  set autoindent                " always set autoindenting on

endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
" WARNING: FA UN MACELLO !
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
	 \ | wincmd p | diffthis
endif


" Abbreviazioni
abbreviate teh the
abbreviate subent subnet


"------------------------------------------------------------------------ 
" Shortcuts
" WARNING: <Leaders> defaults to "\"
"------------------------------------------------------------------------ 

" Don't use Ex mode, use Q for formatting
map Q gq

" \th	|   <F4>
" toggle highlight
nnoremap <Leader>th :set invhls hls?<CR>
nmap <F2> <Leader>th

" \th	|   <F3>
" toggle autoindent
nnoremap <Leader>tai :set invautoindent autoindent?<CR>
nmap <F3> <Leader>tai

" \tp	|   <F4>
" inverte paste e mostra il suo valore
nnoremap <Leader>tp :set invpaste paste?<CR>
nmap <F4> <Leader>tp
imap <F4> <C-O><Leader>tp
set pastetoggle=<F4>

" \tl
" toggle list
nnoremap <Leader>tl :set invlist list?<CR>

" \dd	|   \dmy
" inserisce la data (imap = in modalita' insert)
nmap <Leader>dd :.!date +"\%H:\%M -  "<CR>$
imap <Leader>dmy <C-R>=strftime("%d-%m-%y")<CR>

" \nt
" NERDTree
nmap <Leader>nt :NERDTreeToggle<CR>

" \rot
nmap <Leader>rot ggVGg?

" TRINITY
"" Open and close all the three plugins on the same time
nmap <F8>   :TrinityToggleAll<CR>

" Open and close the srcexpl.vim separately
nmap <F9>   :TrinityToggleSourceExplorer<CR>

" Open and close the taglist.vim separately
nmap <F10>  :TrinityToggleTagList<CR>

" Open and close the NERD_tree.vim separately
nmap <F11>  :TrinityToggleNERDTree<CR> 


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


" ------------------------------------------------------------------------
" PERL FUNCTIONS
" ------------------------------------------------------------------------
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
