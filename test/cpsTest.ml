open OUnit2
open Flax_core.Cps
open Flax_core.Lib.Grammer.CoreGrammer

(* Makes sure that the two programs are equivlent *)
let assert_prog_eq (actual : string) (CoreProg expected) : unit =
  let rec helper actual expected =
    match (actual, expected) with
    | [], [] -> ()
    | CoreDef (a_i, a_exp) :: xs, CoreDef (e_i, e_exp) :: ys ->
        assert_equal a_i e_i;
        assert_equal ~printer:show_core_exp a_exp e_exp;
        helper xs ys
    | _ -> failwith "List lengths are not equal"
  in
  let (CoreProg a_lst) = Flax_core.Lib.Build.run_prog actual in
  helper a_lst expected

let sym_gen_test _ =
  assert_equal "v1" (SymGen.gen_sym ());
  assert_equal "v2" (SymGen.gen_sym ());
  assert_equal "v1" (SymGen.reset () |> SymGen.gen_sym);
  SymGen.reset ()

let cps_function_1 _ =
  assert_prog_eq "(define (f x) 3)"
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
  assert_prog_eq "(define x 10) (define (f x) x)"
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
  assert_prog_eq "(define x 10) (define (f x) (lambda (x) x))"
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
  assert_prog_eq "(define x 10) (define (f x) (lambda (x) (not x)))"
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
  assert_prog_eq "(define x 10) (define (f x) (g (h x)))"
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
                         ( [ "v1" ],
                           CoreAppExp
                             (CoreVarExp "g", [ CoreVarExp "v1"; CoreVarExp "$k$" ]),
                           [] );
                     ] ),
                 [] ) );
       ])

let cps_function_6 _ =
  assert_prog_eq "(define x 10) (define (f x) ((h x) 5))"
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
                         ( [ "v1" ],
                           CoreAppExp
                             (CoreVarExp "v1", [ CoreNumExp 5.; CoreVarExp "$k$" ]),
                           [] );
                     ] ),
                 [] ) );
       ])

let cps_function_7 _ =
  assert_prog_eq "(define x 10) (define (f x) ((h x) (g (k 5))))"
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
                         ( [ "v1" ],
                           CoreAppExp
                             ( CoreVarExp "k",
                               [
                                 CoreNumExp 5.;
                                 CoreLambdaExp
                                   ( [ "v3" ],
                                     CoreAppExp
                                       ( CoreVarExp "g",
                                         [
                                           CoreVarExp "v3";
                                           CoreLambdaExp
                                             ( [ "v2" ],
                                               CoreAppExp
                                                 ( CoreVarExp "v1",
                                                   [ CoreVarExp "v2"; CoreVarExp "$k$" ]
                                                 ),
                                               [] );
                                         ] ),
                                     [] );
                               ] ),
                           [] );
                     ] ),
                 [] ) );
       ])

let suite =
  "Cps tests"
  >::: [
         "SymGen" >:: sym_gen_test;
         "Cps Basic Function 1" >:: cps_function_1;
         "Cps Basic Function 2" >:: cps_function_2;
         "Cps Basic Function 3" >:: cps_function_3;
         "Cps Basic Function 4" >:: cps_function_4;
         "Cps Basic Function 5" >:: cps_function_5;
         "Cps Basic Function 6" >:: cps_function_6;
         "Cps Basic Function 6" >:: cps_function_7;
       ]

let _ = run_test_tt_main suite
