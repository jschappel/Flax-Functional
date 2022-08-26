open Flax_core.Lib.Environment
open Flax_core.Lib.Grammar.CoreGrammar
open TestHelpers
open OUnit2

let alpha_env_contains _ =
  let open AlphaEnvironment in
  let alpha_env =
    new_env ()
    |> ext "x" (CoreNumExp 1.)
    |> ext "y" (CoreNumExp 3.)
    |> ext "z" (CoreNumExp 6.)
  in
  assert_false @@ contains "xx" alpha_env;
  assert_false @@ contains "j" alpha_env;
  assert_true @@ contains "x" alpha_env;
  assert_true @@ contains "y" alpha_env;
  assert_true @@ contains "z" alpha_env

let alpha_env_apply _ =
  let open AlphaEnvironment in
  let alpha_env =
    new_env ()
    |> ext "x" (CoreNumExp 1.)
    |> ext "y" (CoreNumExp 3.)
    |> ext "z" (CoreNumExp 6.)
  in
  assert_equal (apply "x" alpha_env) (Some (CoreNumExp 1.0));
  assert_equal (apply "j" alpha_env) None;
  assert_equal (apply "y" alpha_env) (Some (CoreNumExp 3.0));
  assert_equal (apply "z" alpha_env) (Some (CoreNumExp 6.0))

let alpha_env_add _ =
  let open AlphaEnvironment in
  let alpha_env =
    new_env ()
    |> ext "x" (CoreNumExp 1.)
    |> add "y" (CoreNumExp 3.)
    |> add "z" (CoreNumExp 6.)
  in
  assert_equal (apply "x" alpha_env) (Some (CoreNumExp 1.0));
  assert_equal (apply "j" alpha_env) None;
  assert_equal (apply "y" alpha_env) (Some (CoreNumExp 3.0));
  assert_equal (apply "z" alpha_env) (Some (CoreNumExp 6.0))

let suite =
  "Environment tests"
  >::: [
         "Alpha Env ext" >:: alpha_env_contains;
         "Alpha Env Apply" >:: alpha_env_apply;
         "Alpha Env Add" >:: alpha_env_add;
       ]

let _ = run_test_tt_main suite
