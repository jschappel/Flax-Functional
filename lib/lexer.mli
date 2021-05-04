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

exception LexError of string * int

val lexProgram : string -> token list