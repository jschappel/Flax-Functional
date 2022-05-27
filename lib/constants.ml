(* This file contaiens constants used throughout the compiler *)

(** When given *)
let void_cond_exp = CoreGrammer.CoreVarExp "$$VOID$$"

let placeholder_var = CoreGrammer.CoreVarExp "$V$"

let dummy_var = CoreGrammer.CoreNumExp 42.

let continuation_var = CoreGrammer.CoreVarExp "$k$"
