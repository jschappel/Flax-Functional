open Flax_grammar.CoreGrammar

(** alpha converts a program by creating a unique variable name for each variable in the program *)
val alpha_convert_program : core_prog -> core_prog
