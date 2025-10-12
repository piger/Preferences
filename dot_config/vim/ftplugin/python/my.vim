" Riferimenti:
" http://svn.python.org/projects/python/trunk/Misc/Vim/vimrc
" http://henry.precheur.org/vim/python

setl tabstop=8
setl shiftwidth=4
setl softtabstop=4
setl textwidth=79
setl expandtab
setl smarttab
setl autoindent
setl smartindent
setl omnifunc=pythoncomplete#Complete
setl completeopt=menuone,longest,preview
"setl makeprg=python\ -c\ \"import\ py_compile,sys;\ sys.stderr=sys.stdout;\ py_compile.compile(r'%')\"
"setl efm=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m

" Supporto flake8 - http://pypi.python.org/pypi/flake8
" - prima vede se la error line riporta il numero di colonna
setl errorformat+=%f:%l:%c:\ %m
" - poi prova a matchare senza numero di colonna
setl errorformat+=%f:%l:\ %m
" imposta flake8 come comando per :make
setl makeprg=flake8\ %

setl cinwords=in,elif,else,for,while,try,except,finally,def,class,with
setl encoding=utf-8
setl fileformat=unix
setl wildignore+=*.py[co]

" grazie a compiler/nose.vim e il plugin `makegreen` posso lanciare facilmente
" i test dentro vim. Il keybind di makegreen e' '<Leader>t'.
" compiler nose

" Auto completion with C-<space>
if has("gui_running")
	inoremap <C-space> <C-x><C-o>
else
	inoremap <Nul> <C-x><C-o> 
endif

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

" More syntax highlighting.
let python_highlight_all = 1

" exuberant-ctags
" ctags -R -f ~/.vim/tags/python-2.7 $(python -c 'import sys; print " ".join(sys.path)')
if filereadable(expand("~/.vim/tags/python-2.7"))
	set tags+=$HOME/.vim/tags/python-2.7
endif

" Execute a selection of code (very cool!)
" Use VISUAL to select a range and then hit ctrl-h to execute it.
" https://dev.launchpad.net/UltimateVimPythonSetup
""" if has('python')
""" 	python << EOL
""" import vim
""" def EvaluateCurrentRange():
""" 	eval(compile('\n'.join(vim.current.range),'','exec'),globals())
""" EOL
""" 	map <C-h> :py EvaluateCurrentRange()
""" endif
