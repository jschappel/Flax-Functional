open Token
open CoreProgram

exception ParseError of string

val parse_expression : token list -> expression