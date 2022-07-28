open OUnit2
open Flax_core.FreevarTransformer

(* let occurs_free_in_exp var target_exp = let open Flax_core.Lib in let def_exp =
   "(define test " ^ target_exp ^ ")" in let prog = in assert_equal *)

let occurs_free_basic _ = assert_equal true true

let suite =
  "Freevar Transformer tests" >::: [ "Occurs Free in basic exp" >:: occurs_free_basic ]

let _ = run_test_tt_main suite
