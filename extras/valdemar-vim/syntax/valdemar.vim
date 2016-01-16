" Vim syntax file
" Language: valdemar
" Author: Mateusz Belicki
" Version: 0.1
" Credits: Mateusz Belicki 

if exists("b:current_syntax")
  finish
endif

" if !exists("main_syntax")
"   if version < 600
"     syntax clear
"   elseif exists("b:current_syntax")
"     finish
"   endif
"   let main_syntax = "valdemar"
" endif

syn case match

syn keyword valdemarKeywords fn val mutval ext_c ret if else for while not tuple
syn match valdemarType "\w*_t\>"

syn keyword valdemarCommentTodo TODO todo FIXME fixme TBD contained
syn match valdemarComment "--.*" contains=valdemarCommentTodo

syn region valdemarCharacter start=+'+ skip=+\\\\\|\\"+ end=+'\|$+
syn region valdemarString start=+"+ skip=+\\\\\|\\"+ end=+"\|$+
syn match valdemarNumber "-\=\<\d\+L\=\>\|0[xX][0-9a-fA-F]\+\>"
syn match valdemarFloat /\<-\=\%(\d\+\.\d\+\|\d\+\.\|\.\d\+\)\%([eE][+-]\=\d\+\)\=\>/
syn keyword valdemarBool true flase

let b:current_syntax = "valdemar"

hi def link valdemarCommentTodo     Todo
hi def link valdemarComment         Comment
hi def link valdemarType            Type
hi def link valdemarString          Constant
hi def link valdemarCharacter       Constant
hi def link valdemarBool            Number
hi def link valdemarNumber          Number
hi def link valdemarFloat           Number
hi def link valdemarKeywords        Keyword
" hi def link celBlockCmd    Statement
" hi def link celDesc        PreProc
