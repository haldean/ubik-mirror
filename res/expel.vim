" check out :help group-name for the list of groups

if exists("b:current_syntax")
    finish
endif

syntax match expelBind ":"
highlight link expelBind Delimiter

syntax match expelSubbind "\^"
syntax match expelSubbind "="
highlight link expelSubbind Keyword

let b:current_syntax = "expel"
