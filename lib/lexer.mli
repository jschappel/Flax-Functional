type tokenType = 
    LEFT_PAREN 
    | RIGHT_PAREN 
    | EQUAL
    | PLUS 
    | MINUS 
    | STAR 
    | SLASH 
    | DOT
    | LET 
    | IN 
    | FUN
    | IDENTIFIER of string
    | NUMBER of float

type token = Token of tokenType * int

exception LexError of string * int

val lexProgram : string -> token list