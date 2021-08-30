open Token

exception LexError of string * int

val lexProgram : string -> token list