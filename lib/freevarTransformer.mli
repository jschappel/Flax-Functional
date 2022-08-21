open Flax_grammar.CoreGrammar

(** transforms a program by computing all the free variables *)
val freevar_transfom_program : core_prog -> core_prog

(** determins if the given variable occurs free within the core expression *)
val occurs_free : string -> core_exp -> bool
