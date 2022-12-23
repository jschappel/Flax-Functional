open Flax_grammar.CoreGrammar
open Flax_grammar.CoreGrammar2

(** transforms a program by transforming all lambdas into closures *)
val closure_convert_program : core_prog -> core_prog_phase2
