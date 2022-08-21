(* This file contaiens constants used throughout the compiler *)

(** When given *)
let void_cond_exp = Flax_grammar.CoreGrammar.CoreVarExp "$$VOID$$"

let placeholder_var = Flax_grammar.CoreGrammar.CoreVarExp "$V$"

let dummy_var = Flax_grammar.CoreGrammar.CoreNumExp 42.

let continuation_var = Flax_grammar.CoreGrammar.CoreVarExp "$k$"

let end_cont = Flax_grammar.CoreGrammar.CoreVarExp "end-k"
