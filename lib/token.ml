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
    | STRING of string

type token = Token of tokenType * int