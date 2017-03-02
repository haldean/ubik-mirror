" Vim syntax file
" Language:	Ubik
" Maintainer:	Haldean Brown <me@haldean.org>

" check out :help group-name for the list of groups

if exists("b:current_syntax")
    finish
endif

syntax match ubikBind ":"
syntax match ubikBind "?:"
syntax match ubikBind "!"
syntax match ubikBind "_"
syntax match ubikBind "|"
syntax match ubikBind "'"
highlight link ubikBind Special

syntax match ubikMeta "`[ ]*[*]*"
syntax match ubikMeta "\~"
syntax match ubikMeta "="
syntax match ubikMeta "\^"
highlight link ubikMeta Define

syntax match ubikSubbind "=>"
syntax match ubikSubbind "/>"
syntax match ubikSubbind "->"
syntax match ubikSubbind "?[^:]"
syntax match ubikSubbind "[{}\\\.]"
highlight link ubikSubbind Keyword

syntax region ubikString start=/"/ skip=/\\"/ end=/"/ oneline
highlight default link ubikString String

syntax region ubikComment start=/#/ end=/\n/ oneline
highlight default link ubikComment Comment

syntax match ubikNumber "\v<\d+>"
syntax match ubikNumber "\v<\d+\.\d+>"
highlight default link ubikNumber Number

syntax keyword ubikTypes String Number Boolean
highlight default link ubikTypes Type

setlocal shiftwidth=4
setlocal tabstop=4

let b:current_syntax = "ubik"
