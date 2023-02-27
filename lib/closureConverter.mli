open Flax_grammar.CoreGrammar

(* A Phase2 Program is the same as a Core Program, except the CorePhase2 structurs 
   are also used *)
type core_phase2_prog = core_prog
(** transforms a program by transforming all lambdas into closures *)
val closure_convert_program : core_prog -> core_phase2_prog
