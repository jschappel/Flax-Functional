open OUnit2
open Flax_core.Lib.Grammar.CoreGrammar

(* Makes sure that the given program is equal the the parse tree *)
let assert_prog_eq_parse_tree (actual : string) (CoreProg expected) : unit =
  let rec helper actual expected =
    match (actual, expected) with
    | [], [] -> ()
    | CoreDef (a_i, a_exp) :: xs, CoreDef (e_i, e_exp) :: ys ->
        assert_equal a_i e_i;
        assert_equal ~printer:show_core_exp a_exp e_exp;
        helper xs ys
    | _ -> failwith "List lengths are not equal"
  in
  let open Flax_core.Lib in
  let (CoreProg a_lst) =
    actual |> Parser.parse_program |> Desugarar.desugar_program |> Cps.cps_program
  in
  helper a_lst expected

(* Converts the actual to a cps'ed program, while the expected is expected to be in cps
   form and just converts it to a core program *)
let assert_prog_eq (actual : string) (expected : string) : unit =
  let open Flax_core.Lib in
  let parsed_actual =
    actual |> Parser.parse_program |> Desugarar.desugar_program |> Cps.cps_program
  in
  let parsed_expected = expected |> Parser.parse_program |> Desugarar.desugar_program in
  assert_equal ~printer:show_core_prog parsed_expected parsed_actual

let cps_function_1 _ =
  assert_prog_eq_parse_tree "(define (f x) 3)"
    (CoreProg
       [
         CoreDef
           ( "f",
             CoreLambdaExp
               ( [ "x" ],
                 CoreAppExp (CoreVarExp "f/k", [ CoreVarExp "x"; CoreVarExp "end-k" ]),
                 [] ) );
         CoreDef
           ( "f/k",
             CoreLambdaExp
               ([ "x"; "$k$" ], CoreAppExp (CoreVarExp "$k$", [ CoreNumExp 3. ]), []) );
       ])

let cps_function_2 _ =
  assert_prog_eq_parse_tree "(define x 10) (define (f x) x)"
    (CoreProg
       [
         CoreDef ("x", CoreNumExp 10.);
         CoreDef
           ( "f",
             CoreLambdaExp
               ( [ "x" ],
                 CoreAppExp (CoreVarExp "f/k", [ CoreVarExp "x"; CoreVarExp "end-k" ]),
                 [] ) );
         CoreDef
           ( "f/k",
             CoreLambdaExp
               ([ "x"; "$k$" ], CoreAppExp (CoreVarExp "$k$", [ CoreVarExp "x" ]), []) );
       ])

let cps_function_3 _ =
  assert_prog_eq_parse_tree "(define x 10) (define (f x) (lambda (x) x))"
    (CoreProg
       [
         CoreDef ("x", CoreNumExp 10.);
         CoreDef
           ( "f",
             CoreLambdaExp
               ( [ "x" ],
                 CoreAppExp (CoreVarExp "f/k", [ CoreVarExp "x"; CoreVarExp "end-k" ]),
                 [] ) );
         CoreDef
           ( "f/k",
             CoreLambdaExp
               ( [ "x"; "$k$" ],
                 CoreAppExp
                   ( CoreVarExp "$k$",
                     [
                       CoreLambdaExp
                         ( [ "x"; "$k$" ],
                           CoreAppExp (CoreVarExp "$k$", [ CoreVarExp "x" ]),
                           [] );
                     ] ),
                 [] ) );
       ])

let cps_function_4 _ =
  assert_prog_eq_parse_tree "(define x 10) (define (f x) (lambda (x) (not x)))"
    (CoreProg
       [
         CoreDef ("x", CoreNumExp 10.);
         CoreDef
           ( "f",
             CoreLambdaExp
               ( [ "x" ],
                 CoreAppExp (CoreVarExp "f/k", [ CoreVarExp "x"; CoreVarExp "end-k" ]),
                 [] ) );
         CoreDef
           ( "f/k",
             CoreLambdaExp
               ( [ "x"; "$k$" ],
                 CoreAppExp
                   ( CoreVarExp "$k$",
                     [
                       CoreLambdaExp
                         ( [ "x"; "$k$" ],
                           CoreAppExp (CoreVarExp "$k$", [ CoreNotExp (CoreVarExp "x") ]),
                           [] );
                     ] ),
                 [] ) );
       ])

let cps_function_5 _ =
  assert_prog_eq_parse_tree "(define x 10) (define (f x) (g (h x)))"
    (CoreProg
       [
         CoreDef ("x", CoreNumExp 10.);
         CoreDef
           ( "f",
             CoreLambdaExp
               ( [ "x" ],
                 CoreAppExp (CoreVarExp "f/k", [ CoreVarExp "x"; CoreVarExp "end-k" ]),
                 [] ) );
         CoreDef
           ( "f/k",
             CoreLambdaExp
               ( [ "x"; "$k$" ],
                 CoreAppExp
                   ( CoreVarExp "h",
                     [
                       CoreVarExp "x";
                       CoreLambdaExp
                         ( [ "v0" ],
                           CoreAppExp
                             (CoreVarExp "g", [ CoreVarExp "v0"; CoreVarExp "$k$" ]),
                           [] );
                     ] ),
                 [] ) );
       ])

let cps_function_6 _ =
  assert_prog_eq_parse_tree "(define x 10) (define (f x) ((h x) 5))"
    (CoreProg
       [
         CoreDef ("x", CoreNumExp 10.);
         CoreDef
           ( "f",
             CoreLambdaExp
               ( [ "x" ],
                 CoreAppExp (CoreVarExp "f/k", [ CoreVarExp "x"; CoreVarExp "end-k" ]),
                 [] ) );
         CoreDef
           ( "f/k",
             CoreLambdaExp
               ( [ "x"; "$k$" ],
                 CoreAppExp
                   ( CoreVarExp "h",
                     [
                       CoreVarExp "x";
                       CoreLambdaExp
                         ( [ "v0" ],
                           CoreAppExp
                             (CoreVarExp "v0", [ CoreNumExp 5.; CoreVarExp "$k$" ]),
                           [] );
                     ] ),
                 [] ) );
       ])

let cps_function_7 _ =
  assert_prog_eq_parse_tree "(define x 10) (define (f x) ((h x) (g (k 5))))"
    (CoreProg
       [
         CoreDef ("x", CoreNumExp 10.);
         CoreDef
           ( "f",
             CoreLambdaExp
               ( [ "x" ],
                 CoreAppExp (CoreVarExp "f/k", [ CoreVarExp "x"; CoreVarExp "end-k" ]),
                 [] ) );
         CoreDef
           ( "f/k",
             CoreLambdaExp
               ( [ "x"; "$k$" ],
                 CoreAppExp
                   ( CoreVarExp "h",
                     [
                       CoreVarExp "x";
                       CoreLambdaExp
                         ( [ "v0" ],
                           CoreAppExp
                             ( CoreVarExp "k",
                               [
                                 CoreNumExp 5.;
                                 CoreLambdaExp
                                   ( [ "v2" ],
                                     CoreAppExp
                                       ( CoreVarExp "g",
                                         [
                                           CoreVarExp "v2";
                                           CoreLambdaExp
                                             ( [ "v1" ],
                                               CoreAppExp
                                                 ( CoreVarExp "v0",
                                                   [ CoreVarExp "v1"; CoreVarExp "$k$" ]
                                                 ),
                                               [] );
                                         ] ),
                                     [] );
                               ] ),
                           [] );
                     ] ),
                 [] ) );
       ])

let cps_function_factorial _ =
  assert_prog_eq "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))"
    "(define fact (lambda (n) (fact/k n end-k)))
     (define fact/k (lambda (n $k$) 
        (if (= n 0)
          ($k$ 1) 
          (fact/k (- n 1) (lambda (v0) ($k$ (* n v0)))))))"

let cps_function_insertion_sort _ =
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
            (cons (car L) (insert x (cdr L))))))"
    "
  (define insort (lambda (L) (insort/k L end-k)))
  (define insort/k
    (lambda (L $k$)
      (if (null? L)
        ($k$ L)
        (insort/k (cdr L) (lambda (v0) (insert/k (car L) v0 $k$))))))

  (define insert (lambda (x L) (insert/k x L end-k)))
  (define insert/k
    (lambda (x L $k$)
      (if (null? L)
        ($k$ (cons x (emptylist)))
        (if (<= x (car L))
          ($k$ (cons x L))
          (insert/k x (cdr L) (lambda (v1) ($k$ (cons (car L) v1))))))))"

let suite =
  "Cps tests"
  >::: [
         "Cps Basic Function 1" >:: cps_function_1;
         "Cps Basic Function 2" >:: cps_function_2;
         "Cps Basic Function 3" >:: cps_function_3;
         "Cps Basic Function 4" >:: cps_function_4;
         "Cps Basic Function 5" >:: cps_function_5;
         "Cps Basic Function 6" >:: cps_function_6;
         "Cps Basic Function 7" >:: cps_function_7;
         "Cps Basic Function 8" >:: cps_function_factorial;
         "Cps Basic Function 9" >:: cps_function_insertion_sort;
       ]

let _ = run_test_tt_main suite
