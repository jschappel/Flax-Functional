type tokenType = 
    LEFT_PAREN 
    | RIGHT_PAREN 
    | EQUAL
    | PLUS 
    | MINUS 
    | STAR 
    | SLASH 
    | DOT
    | NUMBER
    | LET 
    | IN 
    | FUN
    | IDENTIFIER

type token = Token of string * tokenType * int

val display : token -> string

val equal : token -> token -> bool