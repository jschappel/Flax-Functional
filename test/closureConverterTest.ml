open OUnit2
open Flax_core.Lib

let convert (prog : string) =
  prog
  |> Parser.parse_program
  |> Desugarar.desugar_program
  |> ClosureConverter.closure_convert_program

let assert_prog_eq p1 p2 = assert_equal ~printer:Grammar.CoreGrammar.show_core_prog p1 p2

let closure_convert_exp _ =
  (* assert_prog_eq (convert "(define x 10)") (convert "(define x (lambda (x) (+ x a b)))") *)
  assert_equal true true

let suite =
  "Closure Converter tests"
  >::: [
         "Closure Convert basic Expression" >:: closure_convert_exp;
         (* "Alpha Convert Factorial" >:: alpha_convert_factorial; *)
         (* "Alpha Convert Insort" >:: alpha_convert_insort; *)
       ]

let _ = run_test_tt_main suite
