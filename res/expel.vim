" check out :help group-name for the list of groups

if exists("b:current_syntax")
    finish
endif

syntax match expelBind ":"
highlight link expelBind Delimiter

syntax match expelSubbind "\^"
syntax match expelSubbind "="
highlight link expelSubbind Keyword

syntax region expelString start=/"/ skip=/\\"/ end=/"/ oneline
highlight default link expelString String

syntax region expelComment start=/#/ end=/\n/ oneline
highlight default link expelComment Comment

syntax match expelNumber "\v<\d+>"
syntax match expelNumber "\v<\d+\.\d+>"
highlight default link expelNumber Number

let b:current_syntax = "expel"
