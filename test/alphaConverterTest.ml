open OUnit2

let assert_prog_eq (actual : string) (expected : string) =
  let open Flax_core.Lib in
  let desugared_expected =
    expected |> Parser.parse_program |> Desugarar.desugar_program
  in
  let alpha_prog =
    actual
    |> Parser.parse_program
    |> Desugarar.desugar_program
    |> Cps.cps_program
    |> Flax_core.AlphaConverter.alpha_convert_program
  in
  assert_equal ~printer:Grammar.CoreGrammar.show_core_prog desugared_expected alpha_prog

  (* (define f (lambda (x) (f/k x end-k)))
     (define f/k (lambda (x $k$) ($k$ 3))) 
  *)
let alpha_convert_function_1 _ =
  assert_prog_eq "(define (f x) 3)"
    "(define __D0__ (lambda (__V0__) (__D1__ __V0__ end-k)))
     (define __D1__ (lambda (__V1__ __V2__) (__V2__ 3)))"

let alpha_convert_function_2 _ =
  assert_prog_eq "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))"
    "(define (*D*0 *V*0) (*D*1 *V*0 end-k))
     (define (*D*1 *V*1 *V*2)
      (if (= *V*1 0)
        (*V*2 1)
        (*D*1 (- *V*1 1)
              (lambda (*V*3) (*V*2 (* *V*1 *V*3))))))"

let suite =
  "Alpha Converter tests"
  >::: [
         "Alpha Convert Function 1" >:: alpha_convert_function_1;
         (* "Alpha Convert Factorial" >:: alpha_convert_function_2; *)
       ]

let _ = run_test_tt_main suite
