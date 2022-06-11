(* This file contaiens constants used throughout the compiler *)

(** When given *)
let void_cond_exp = Grammer.CoreGrammer.CoreVarExp "$$VOID$$"

let placeholder_var = Grammer.CoreGrammer.CoreVarExp "$V$"

let dummy_var = Grammer.CoreGrammer.CoreNumExp 42.

let continuation_var = Grammer.CoreGrammer.CoreVarExp "$k$"

let end_cont = Grammer.CoreGrammer.CoreVarExp "end-k"
