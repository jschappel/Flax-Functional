open OUnit2
open Lib.Enviroment
open Lib

let enviroment_empty_env _ = assert_equal EmptyEnv empty_env
let show_optional_value = function Some v -> show_value v | None -> "None"

let enviroment_ext_env_empty _ =
  let vals = [ ("a", BoolVal true); ("b", NumVal 10.0) ] in
  let new_env = ext_env vals EmptyEnv in
  assert_equal new_env @@ ExtEnv (vals, EmptyEnv)

let enviroment_ext_env_non_empty _ =
  let vals = [ ("a", BoolVal true); ("b", NumVal 10.0) ] in
  let old_env = ext_env [ ("j", BoolVal false) ] EmptyEnv in
  let new_env = ext_env vals old_env in
  assert_equal new_env @@ ExtEnv (vals, old_env)

let enviroment_get_env_value_exists _ =
  let env =
    ext_env [ ("a", BoolVal true); ("b", NumVal 10.0) ]
    @@ ext_env [ ("j", NumVal 1.0) ] EmptyEnv
  in
  assert_equal ~cmp:(Option.equal equal_value) (get_env_value "j" env)
    (Some (NumVal 1.0))

let enviroment_get_env_value _ =
  let env =
    ext_env [ ("a", BoolVal true); ("b", NumVal 10.0) ]
    @@ ext_env [ ("j", NumVal 1.0) ] EmptyEnv
  in
  assert_equal (get_env_value "aa" env) None

let enviroment_get_env_value2 _ =
  let env =
    ext_env [ ("a", BoolVal true); ("b", NumVal 10.0) ]
    @@ ext_env_rec
         [
           ( "add",
             [ "x"; "y" ],
             BinaryExpr (PLUS, LiteralExpr (Ident "x"), LiteralExpr (Ident "y"))
           );
         ]
         EmptyEnv
  in
  assert_equal (get_env_value "sub" env) None

let enviroment_get_env_value3 _ =
  let fbody =
    BinaryExpr (PLUS, LiteralExpr (Ident "x"), LiteralExpr (Ident "y"))
  in
  let bound_vars = [ "x"; "y" ] in
  let inner_env = ext_env_rec [ ("add", bound_vars, fbody) ] EmptyEnv in
  let outter_env =
    ext_env [ ("a", BoolVal true); ("b", NumVal 10.0) ] @@ inner_env
  in
  assert_equal ~printer:show_optional_value ~cmp:(Option.equal equal_value)
    (Some (ProcVal (bound_vars, fbody, inner_env)))
    (get_env_value "add" outter_env)

let suite =
  "Enviroment"
  >::: [
         "Empty Env" >:: enviroment_empty_env;
         "ExtEnv when EmptyEnv" >:: enviroment_ext_env_empty;
         "ExtEnv when exitsing env" >:: enviroment_ext_env_non_empty;
         "GetEnvValue when exists" >:: enviroment_get_env_value_exists;
         "GetEnvValue when does not exist" >:: enviroment_get_env_value;
         "GetEnvValue when does not exist ExtEnvRec"
         >:: enviroment_get_env_value2;
         "GetEnvValue when exists ExtEnvRec" >:: enviroment_get_env_value3;
       ]

let () = run_test_tt_main suite
