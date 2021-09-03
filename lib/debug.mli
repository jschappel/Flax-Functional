open Token
open CoreProgram

(* For debugging Token lists *)
val tokenType_to_string : tokenType -> string

val token_list_to_string : token list -> string

val expr_to_string : expression -> string