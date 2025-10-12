" Vim syntax file
" Language:	TE (SELinux Type Enforcement Policy)
" Author:	Thomas Bleher <ThomasBleher@gmx.de>
" Lastchange:	2004 Dec 25
" URL:		http://www.cip.informatik.uni-muenchen.de/~bleher/selinux/te.vim 
" uses m4.vim, some code taken from cpp.vim
"
" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" First load the M4 syntax
if version < 600
  so <sfile>:p:h/m4.vim
else
  runtime! syntax/m4.vim
  unlet b:current_syntax
endif

syn sync	minlines=100

" define the te syntax
syn keyword	teStatement	allow dontaudit auditallow neverallow type attribute type_change user role type_transition dominance alias types roles typealias bool typeattribute
syn keyword	teTodo		TODO XXX FIXME contained
syn match	teDescription	/\<\(Authors\=\>\|X[-_A-Za-z]*-Packages:\|DESC\>\|Depends:\).*/ contained
syn match	teComment	/#.*/ contains=teTodo,teDescription
syn match	teNegated	/[-~]\(\w\|\$\)\+/
syn region	teNegated	start="\~{" end="}" contains=@m4Top
syn region	teConditional	start="if (.*) {" end="}" contains=@m4Top
syn match	teSpecial	/\*/

" highlight teStatements and teComments inside of m4-macros
syn cluster	m4Top		contains=m4Comment,m4Constants,m4Special,m4Variable,m4String,m4Paren,m4Command,m4Statement,m4Function,teStatement,teComment,teNegated,teConditional
" change m4Type to allow lowercase-macros
syn region	m4Function	matchgroup=m4Type      start="\<[_A-Za-z]\w*("he=e-1 end=")" contains=@m4Top

" Default highlighting
if version >= 508 || !exists("did_te_syntax_inits")
  if version < 508
    let did_te_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink teStatement	Statement
  HiLink teTodo		Todo
  HiLink teDescription	Underlined
  HiLink teComment	Comment
  HiLink teNegated	Special
  HiLink teSpecial	Special
  HiLink teConditional	String
  delcommand HiLink
endif

let b:current_syntax = "te"

" arch-tag: a0a77edf-bed7-43ca-a19e-6279177a9de2
