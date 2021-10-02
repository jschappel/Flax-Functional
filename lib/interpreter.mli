open CoreProgram
open Enviroment

val value_of: expression -> enviroment -> value

val occurs_free: string -> expression -> bool

val make_free_var_env:  enviroment -> expression -> pair list -> enviroment