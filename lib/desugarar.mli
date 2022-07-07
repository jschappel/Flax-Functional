open Grammer.SourceGrammer
open Grammer.CoreGrammer

(** Desugars the program into the core grammar *)
val desugar_program : program -> core_prog
