" Riferimenti:
" http://svn.python.org/projects/python/trunk/Misc/Vim/vimrc
" http://henry.precheur.org/vim/python

setl tabstop=8
setl shiftwidth=4
setl softtabstop=4
setl textwidth=80
setl expandtab
setl smarttab
setl autoindent
setl smartindent
setl omnifunc=pythoncomplete#Complete
setl completeopt=menuone,longest,preview
setl makeprg=python\ -c\ \"import\ py_compile,sys;\ sys.stderr=sys.stdout;\ py_compile.compile(r'%')\"
setl efm=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m
setl cinwords=if,elif,else,for,while,try,except,finally,def,class

" I commenti in python, con "smartindent", vanno sempre a inizio riga;
" questo fixa 'sto behaviour
" http://stackoverflow.com/questions/2360249/vim-automatically-removes-indentation-on-python-comments
inoremap <buffer> # X#
" (<buffer> e' per renderlo valido solo per il file python aperto e non per
" gli altri)

" setl foldcolumn=2
"setl foldmethod=indent
" Apre di default tutti i fold
setl foldlevel=100

" line number con la GUI
"" if has("gui_running")
""     setl number
"" endif
