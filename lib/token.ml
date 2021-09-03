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
    | IF
    | THEN
    | ELSE
    | IDENTIFIER of string
    | NUMBER of float
    | STRING of string
    | BOOL of bool

type token = Token of tokenType * int