open Grammar.SourceGrammar
open Grammar.CoreGrammar

(** Desugars the program into the core grammar *)
val desugar_program : program -> core_prog
