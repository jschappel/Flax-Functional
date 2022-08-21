open Flax_grammar.SourceGrammar
open Flax_grammar.CoreGrammar

(** Desugars the program into the core grammar *)
val desugar_program : program -> core_prog
