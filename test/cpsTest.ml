open OUnit2
open Flax_core.Cps

let sym_gen_test _ =
  assert_equal (SymGen.gen_sym ()) "v1";
  assert_equal (SymGen.gen_sym ()) "v2";
  assert_equal (SymGen.reset () |> SymGen.gen_sym) "v1"

let suite = "Cps tests" >::: [ "SymGen" >:: sym_gen_test ]

let _ = run_test_tt_main suite
