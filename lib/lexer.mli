open Token

exception LexError of string * int

type line = string
type program = line list

(* Convets a line of a program into a token list *)
val lex_line : ?line_num:int -> line -> token list

(* Converts a program into a token list *)
val lex_program : program -> token list
