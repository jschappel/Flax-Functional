open OUnit2
open TestHelpers
open Lib
open Lib.Enviroment

let run (p : string) : value =
  EmptyEnv |> value_of @@ parse_expression @@ lexProgram p

let parse ?(debug = false) (s : string) : expression =
  let e = parse_expression @@ lexProgram s in
  if debug then (
    print_endline @@ show_expression e;
    e)
  else e
(* let _d = print_endline @@ show_expression e in *)

let addition _ =
  assert_equal ~printer:show_value (run "1 + 3 + 50.5") (NumVal 54.5)

let subtraction _ =
  assert_equal ~printer:show_value (run "1 - 3 - 5") (NumVal (-7.0))

let multiplication _ =
  assert_equal ~printer:show_value (run "1 * 50 * 100") (NumVal 5000.0)

let division _ =
  assert_equal ~printer:show_value (run "100 / 10 / 2") (NumVal 5.0)

let ord_of_ops _ =
  assert_equal ~printer:show_value (run "1 + 2 * 20 / 10 - 7") (NumVal (-2.0))

let less _ = assert_equal ~printer:show_value (run "6 < 7") (BoolVal true)
let greater _ = assert_equal ~printer:show_value (run "60 > 7") (BoolVal true)
let less_eq _ = assert_equal ~printer:show_value (run "6 <= 6") (BoolVal true)

let greater_eq _ =
  assert_equal ~printer:show_value (run "60 >= 60") (BoolVal true)

let let_expr _ =
  assert_equal ~printer:show_value (run "let x = 20 in x + 6") (NumVal 26.0)

let multi_let_expr _ =
  assert_equal ~printer:show_value
    (run "let x = 20, y = 30 in x > y")
    (BoolVal false)

let and_expr _ =
  assert_equal ~printer:show_value (run "true and false") (BoolVal false)

let or_expr _ =
  assert_equal ~printer:show_value (run "true or false") (BoolVal true)

let func_no_args _ =
  assert_equal ~printer:show_value
    (run "let truth = fn => true in truth() or false")
    (BoolVal true)

let func_arg _ =
  assert_equal ~printer:show_value
    (run "let add10 = fn y => 10 + y in add10(70)")
    (NumVal 80.0)

let func_args _ =
  assert_equal ~printer:show_value
    (run "let add = fn x, y => x + y in add(10, 20)")
    (NumVal 30.0)

let nested_funcs _ =
  assert_equal ~printer:show_value
    (run
       "let x = 200 \n\
       \    in let f = fn z => z - x\n\
       \     in let x = 100 \n\
       \      in let g = fn z => z - x \n\
       \        in f(1) - g(1)")
    (NumVal (-100.0))

let suite =
  "Interpreter"
  >::: [
         "Addition" >:: addition;
         "Subtraction" >:: subtraction;
         "Multiplication" >:: multiplication;
         "Division" >:: division;
         "Order of Operations" >:: ord_of_ops;
         "Less Then" >:: less;
         "Greater Then" >:: greater;
         "Less hen or Equal" >:: less_eq;
         "Greater Then or Equal" >:: greater_eq;
         "Add" >:: and_expr;
         "Or" >:: or_expr;
         "Let Expression" >:: let_expr;
         "Multi Let Expression" >:: multi_let_expr;
         "Function No Args" >:: func_no_args;
         "Function Single Args" >:: func_arg;
         "Function Multipule Args" >:: func_args;
         "Closure1" >:: nested_funcs;
       ]

(* Occures Free Checks Below *)
let occurs_free1 _ = assert_false @@ occurs_free "x" (parse "fn x, y => x + z")
let occurs_free2 _ = assert_true @@ occurs_free "z" (parse "fn x, y => x + z")
let occurs_free3 _ = assert_true @@ occurs_free "x" (parse "x + y")

let occurs_free4 _ =
  assert_true @@ occurs_free "x" (parse "if x > y then -1 else 1")

let occurs_free5 _ =
  assert_true
  @@ occurs_free "z" (parse "let add = fn x, y => x + y + z in add(10, 20)")

let occurs_free6 _ =
  assert_false
  @@ occurs_free "z" (parse "let add = fn z, y => x + y + z in add(10, 20)")

let occurs_free7 _ =
  assert_true @@ occurs_free "z" (parse "let x = 20, y = 20 + z in x + y")

let env1 =
  ExtEnv
    ( [ ("x", NumVal 10.0); ("y", NumVal 5.0) ],
      ExtEnv ([ ("xx", NumVal 10.0); ("yy", NumVal 500.0) ], EmptyEnv) )

let free_var_env1 _ =
  assert_equal ~printer:show_env
    (ExtEnv ([ ("x", NumVal 10.0); ("yy", NumVal 500.0) ], EmptyEnv))
    (make_free_var_env env1 (parse "fn xx, y => x + y + xx + yy") [])

let suite2 =
  "Occurs Free"
  >::: [
         "Occurs Free 1" >:: occurs_free1;
         "Occurs Free 2" >:: occurs_free2;
         "Occurs Free 3" >:: occurs_free3;
         "Occurs Free 4" >:: occurs_free4;
         "Occurs Free 5" >:: occurs_free5;
         "Occurs Free 6" >:: occurs_free6;
         "Occurs Free 7" >:: occurs_free7;
         "Free Var Env" >:: free_var_env1;
       ]

let () =
  run_test_tt_main suite;
  run_test_tt_main suite2
