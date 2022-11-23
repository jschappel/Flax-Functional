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

let alpha_convert_function_1 _ =
  assert_prog_eq "(define (f x) 3)"
    "(define __D0__ (lambda (__V0__) (__D1__ __V0__ end-k)))
     (define __D1__ (lambda (__V1__ __K0__) (__K0__ 3)))"

let alpha_convert_factorial _ =
  assert_prog_eq "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))"
    "(define (__D0__ __V0__) (__D1__ __V0__ end-k))
     (define (__D1__ __V1__ __K0__)
      (if (=  __V1__ 0)
        (__K0__ 1)
        (__D1__ (- __V1__ 1)
              (lambda (__V2__) (__K0__ (* __V1__ __V2__))))))"

let alpha_convert_insort _ =
  assert_prog_eq
    "
  (define (insort L)
    (if (null? L)
        L
        (insert (car L) (insort (cdr L)))))

  (define (insert x L)
    (if (null? L)
        (list x)
        (if (<= x (car L))
            (cons x L)
            (cons (car L) (insert x (cdr L))))))
  "
    "
  (define __D0__ (lambda (__V0__) (__D1__ __V0__ end-k)))
  (define __D1__ 
    (lambda (__V1__ __K0__)
      (if (null? __V1__)
        (__K0__ __V1__)
        (__D1__  (cdr __V1__) (lambda (__V2__) (__D3__ (car __V1__) __V2__ __K0__))))))
  
    (define __D2__ (lambda (__V3__ __V4__) (__D3__ __V3__ __V4__ end-k)))
    (define __D3__
      (lambda (__V5__ __V6__ __K1__)
        (if (null? __V6__)
          (__K1__ (cons __V5__ (emptylist)))
          (if (<= __V5__ (car __V6__))
            (__K1__ (cons __V5__ __V6__))
            (__D3__ __V5__ (cdr __V6__) (lambda (__V7__) (__K1__ (cons (car __V6__) __V7__))))))))"

let suite =
  "Alpha Converter tests"
  >::: [
         "Alpha Convert Function 1" >:: alpha_convert_function_1;
         "Alpha Convert Factorial" >:: alpha_convert_factorial;
         "Alpha Convert Insort" >:: alpha_convert_insort;
       ]

let _ = run_test_tt_main suite
