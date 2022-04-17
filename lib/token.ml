type tokenType =
  | LEFT_PAREN
  | RIGHT_PAREN
  | COMMA
  | ARROW
  | EQ
  | EQ_EQ
  | GT
  | GT_EQ
  | LT
  | LT_EQ
  | NOT_EQ
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | DOT
  | LET
  | LETREC
  | IN
  | FUN
  | NOT
  | AND
  | OR
  | IF
  | THEN
  | ELSE
  | IDENTIFIER of string
  | NUMBER of float
  | STRING of string
  | BOOL of bool
[@@deriving show, eq]

type token = Token of tokenType * int [@@deriving show]
