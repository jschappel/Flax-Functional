open Flax_grammar.CoreGrammar

(** Converts a program to Continuation Passing style *)
val cps_program : core_prog -> core_prog
