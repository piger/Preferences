" vimrc: server-specific (GNU/Linux)

if !isdirectory(expand("~/.vim/backups"))
    call mkdir(expand("~/.vim/backups"), "", 0700)
endif

set nocompatible		" si comporta da vim e non da vi :)
set backspace=indent,eol,start	" permette il backspace sempre
set backup			" crea una copia di backup prima di sovrascrivere
set backupdir=~/.vim/backups,.	" directory per i file di backup
set backupskip=/tmp/*,/private/tmp/*	" Fixa il problema di vim e i file crontab (anche su OSX)
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
set path=./**,**		" i path per il comando :find, :tabfind, etc (comodo!)
set laststatus=2		" mostra sempre la riga di status con le info sul file
set nomodeline			" NON uso le modlines, ma le securemodlines tramite plugin
set report=0			" Mostra sempre il numero di righe modificate da un comando   
set ruler				" mostra la posizione del cursore in basso a destra
set scrolloff=5			" scrolla con un context di 3 righe
set sidescroll=3		" scrolla lateralmente con un context di 3 righe
set showcmd				" mostra comandi parziali mentre vengono digitati
set noshowmatch			" NON mostrare la parentesi corrispettiva quando ne inserisci una
set showmode			" mostra un messaggio se in modalita' insert/visual/replace
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
set viminfo='100,<50,s5,/20,:20

set tabstop=4                   " numero di spazi per <Tab>
set shiftwidth=4                " numero di spazi per 'step' di indent
set softtabstop=4
set noexpandtab

syntax on
filetype plugin indent on
