open OUnit2
open Lib

let run p =
  EmptyEnv 
  |> value_of @@ parse_expression @@ lexProgram p

let addition _ = assert_equal
  ~printer:show_value
  (run "1 + 3 + 50.5") 
  (NumVal(54.5))

let subtraction _ = assert_equal
  ~printer:show_value
  (run "1 - 3 - 5")
  (NumVal(-7.0))

let multiplication _ = assert_equal
  ~printer:show_value
  (run "1 * 50 * 100")
  (NumVal(5000.0))

let division _ = assert_equal
  ~printer:show_value
  (run "100 / 10 / 2")
  (NumVal(5.0))

let ord_of_ops _ = assert_equal
  ~printer:show_value
  (run "1 + 2 * 20 / 10 - 7")
  (NumVal(-2.0))

let less _ = assert_equal
~printer:show_value
(run "6 < 7")
(BoolVal(true))

let greater _ = assert_equal
~printer:show_value
(run "60 > 7")
(BoolVal(true))

let less_eq _ = assert_equal
~printer:show_value
(run "6 <= 6")
(BoolVal(true))

let greater_eq _ = assert_equal
~printer:show_value
(run "60 >= 60")
(BoolVal(true))

let let_expr _ = assert_equal
  ~printer:show_value
  (run "let x = 20 in x + 6")
  (NumVal(26.0))

let multi_let_expr _ = assert_equal
  ~printer:show_value
  (run "let x = 20, y = 30 in x > y")
  (BoolVal(false))

let and_expr _ = assert_equal
  ~printer:show_value
  (run "true and false")
  (BoolVal(false))

let or_expr _ = assert_equal
  ~printer:show_value
  (run "true or false")
  (BoolVal(true))

let func_no_args _ = assert_equal
  ~printer:show_value
  (run "let truth = fn => true in truth() or false")
  (BoolVal(true))

let func_arg _ = assert_equal
  ~printer:show_value
  (run "let add10 = fn y => 10 + y in add10(70)")
  (NumVal(80.0))

let func_args _ = assert_equal
  ~printer:show_value
  (run "let add = fn x, y => x + y in add(10, 20)")
  (NumVal(30.0))

let nested_funcs _ = assert_equal
  ~printer:show_value
  (run 
  "let x = 200 
    in let f = fn z => z - x
     in let x = 100 
      in let g = fn z => z - x 
        in f(1) - g(1)")
  (NumVal(-100.0))


let suite =
  "Interpreter" >:::
   ["Addition" >:: addition;
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
  ;;

let () =
  run_test_tt_main suite
;;