open Grammar.CoreGrammar

(** transforms a program by computing all the free variables *)
val freevar_transfom_program : core_prog -> core_prog
