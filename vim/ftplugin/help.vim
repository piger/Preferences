" Binding per facilitare la navigazione nell'help di Vim
" ENTER ha la stessa funzione di C-] per seguire un link
nnoremap <buffer> <CR> <C-]>
" BACKSPACE ha la stessa funzione di C-t per tornare indietro.
nnoremap <buffer> <BS> <C-T>

" Now you can press o to go to next place where an option link is, or s if you
" want to go to the next subject link.
nnoremap <buffer> o /''[a-z]\{2,\}''<CR>
nnoremap <buffer> O ?''[a-z]\{2,\}''<CR>
nnoremap <buffer> s /\|\S\+\|<CR>
nnoremap <buffer> S ?\|\S\+\|<CR>

" (da "Hacking Vim")
