open CoreProgram
open Enviroment.Enviroment

exception InterpreterError of string

val value_of : expression -> env -> value
val occurs_free : string -> expression -> bool
val make_free_var_env : env -> expression -> pair list -> env
