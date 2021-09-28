open Lib
open OUnit2
open Base 

let opt_to_str = function 
| Some(v) -> show_value v
| None -> "NONE"


let e1 = ExtEnv([("x", NumVal(10.0));("y", BoolVal(true))], 
            ExtEnv([("xx", NumVal(10.0));("yy", BoolVal(true))], EmptyEnv))

let enviroment_lookup _ : unit = assert_equal
  ~cmp:(fun x y -> equal_option equal_value x y)
  ~printer:opt_to_str
  (Some(NumVal(10.0)))
  (get_value e1 "x")

let enviroment_lookup2 _ : unit = assert_equal
  ~cmp:(fun x y -> equal_option equal_value x y)
  ~printer:opt_to_str
  (Some(BoolVal(true)))
  (get_value e1 "yy")


let suite =
  "Enviroment" >:::
   ["Root Lev Lookup" >:: enviroment_lookup;
    "Nested Lookup" >:: enviroment_lookup2;]
  ;;
  
let () =
  run_test_tt_main suite
;;