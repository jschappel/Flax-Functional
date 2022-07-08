(* This file contaiens constants used throughout the compiler *)

(** When given *)
let void_cond_exp = Grammar.CoreGrammar.CoreVarExp "$$VOID$$"

let placeholder_var = Grammar.CoreGrammar.CoreVarExp "$V$"

let dummy_var = Grammar.CoreGrammar.CoreNumExp 42.

let continuation_var = Grammar.CoreGrammar.CoreVarExp "$k$"

let end_cont = Grammar.CoreGrammar.CoreVarExp "end-k"
