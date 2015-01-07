if exists("b:current_syntax")
    finish
endif

syntax keyword gpcKeyword int bool if else for return seq par string
highlight link gpcKeyword Keyword

syntax match gpcComment "\v#.*$"
highlight link gpcComment Comment

"String Literals
syntax region gpcString start=/\v"/ skip=/\v\\./ end=/\v"/
highlight link gpcString String

"Operators
syntax match gpcOperator "\v\*"
syntax match gpcOperator "\v/"
syntax match gpcOperator "\v\+"
syntax match gpcOperator "\v-"
syntax match gpcOperator "\v\|\|"
syntax match gpcOperator "\v\\|"
syntax match gpcOperator "\v\^"
syntax match gpcOperator "\v\&"
syntax match gpcOperator "\v\&&"

highlight link gpcOperator Operator

let b:current_syntax = "gpc"
