open Lib
open OUnit2
open Core

let opt_to_str = function 
| Some(v) -> val_to_string v
| None -> "NONE"


let e1 = ExtEnv([("x", Num(10.0));("y", Bool(true))], 
            ExtEnv([("xx", Num(10.0));("yy", Bool(true))], EmptyEnv))

let enviroment_lookup _ : unit = assert_equal
  ~cmp:(fun x y -> equal_option Lib.cmp x y)
  ~printer:opt_to_str
  (Some(Num(10.0)))
  (get_value e1 "x")

let enviroment_lookup2 _ : unit = assert_equal
  ~cmp:(fun x y -> equal_option Lib.cmp x y)
  ~printer:opt_to_str
  (Some(Bool(true)))
  (get_value e1 "yy")




let suite =
  "Enviroment" >:::
   ["Root Lev Lookup" >:: enviroment_lookup;
    "Nested Lookup" >:: enviroment_lookup2;]
  ;;
  
let () =
  run_test_tt_main suite
;;