open OUnit2
open Lib

let run p =
  EmptyEnv 
  |> value_of @@ parse_expression @@ lexProgram p

let addition _ = assert_equal
  ~printer:val_to_string
  (run "1 + 3 + 50.5") 
  (NumVal(54.5))

let subtraction _ = assert_equal
  ~printer:val_to_string
  (run "1 - 3 - 5")
  (NumVal(-7.0))

let multiplication _ = assert_equal
  ~printer:val_to_string
  (run "1 * 50 * 100")
  (NumVal(5000.0))

let division _ = assert_equal
  ~printer:val_to_string
  (run "100 / 10 / 2")
  (NumVal(5.0))

let ord_of_ops _ = assert_equal
  ~printer:val_to_string
  (run "1 + 2 * 20 / 10 - 7")
  (NumVal(-2.0))

let let_expr _ = assert_equal
  ~printer:val_to_string
  (run "let x = 20 in x + 6")
  (NumVal(26.0))

let multi_let_expr _ = assert_equal
  ~printer:val_to_string
  (run "let x = 20, y = 30 in x > y")
  (BoolVal(false))

let suite =
  "Interpreter" >:::
   ["Addition" >:: addition;
    "Subtraction" >:: subtraction;
    "Multiplication" >:: multiplication;
    "Division" >:: division;
    "Order of Operations" >:: ord_of_ops;
    "Let Expression" >:: let_expr;
    "Multi Let Expression" >:: multi_let_expr;
    ]
  ;;

let () =
  run_test_tt_main suite
;;