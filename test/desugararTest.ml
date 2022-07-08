open OUnit2
open Flax_core.Lib.Grammar.CoreGrammar
open Flax_core.Desugarar

(* Checks if two core programs are equivalent to one another *)
let core_prog_eq (s : string) (v : 'a) : unit =
  let run x = Flax_core.Lib.Parser.parse_program x |> desugar_program in
  assert_equal ~printer:show_core_prog v (run s)

(* Checks if two core expressions are equivalent to one another, ignoring the outter
   define expression Note: This can only be used to check a program with one definiton *)
let core_exp_eq (s : string) (v : 'a) : unit =
  let run x = Flax_core.Lib.Parser.parse_program x |> desugar_program in
  match run s with
  | CoreProg [ CoreDef (_, exp) ] -> assert_equal ~printer:show_core_exp v exp
  | _ ->
      failwith
        "This Function can only be used to check a program wiht a single definition"

let desugar_def_exps _ =
  core_prog_eq "(define (a) true)"
    (CoreProg [ CoreDef ("a", CoreLambdaExp ([], CoreBoolExp true, [])) ]);
  core_prog_eq "(define (add x y) (+ x y))"
    (CoreProg
       [
         CoreDef
           ( "add",
             CoreLambdaExp
               ( [ "x"; "y" ],
                 CoreAppExp (CoreVarExp "+", [ CoreVarExp "x"; CoreVarExp "y" ]),
                 [] ) );
       ])

let desugar_let_exps _ =
  core_exp_eq "(define x (let ((x 10) (y 20)) (+ x y)))"
    (CoreAppExp
       ( CoreLambdaExp
           ( [ "x"; "y" ],
             CoreAppExp (CoreVarExp "+", [ CoreVarExp "x"; CoreVarExp "y" ]),
             [] ),
         [ CoreNumExp 10.; CoreNumExp 20. ] ));
  core_exp_eq "(define x (let ((x 10) (y 20)) (let ((z 30)) (+ x y z))))"
    (CoreAppExp
       ( CoreLambdaExp
           ( [ "x"; "y" ],
             CoreAppExp
               ( CoreLambdaExp
                   ( [ "z" ],
                     CoreAppExp
                       (CoreVarExp "+", [ CoreVarExp "x"; CoreVarExp "y"; CoreVarExp "z" ]),
                     [] ),
                 [ CoreNumExp 30. ] ),
             [] ),
         [ CoreNumExp 10.; CoreNumExp 20. ] ))

let desugar_cond_exps _ =
  core_exp_eq "(define x (cond (else 20)))"
    (CoreIfExp (CoreBoolExp true, CoreNumExp 20.0, Flax_core.Constants.void_cond_exp));
  core_exp_eq "(define x (cond ((eq? 10 y) true) (else false)))"
    (CoreIfExp
       ( CoreAppExp (CoreVarExp "eq?", [ CoreNumExp 10.; CoreVarExp "y" ]),
         CoreBoolExp true,
         CoreBoolExp false ));
  core_exp_eq "(define x (cond ((eq? 10 y) true) ((eq? 20 y) false)))"
    (CoreIfExp
       ( CoreAppExp (CoreVarExp "eq?", [ CoreNumExp 10.; CoreVarExp "y" ]),
         CoreBoolExp true,
         CoreIfExp
           ( CoreAppExp (CoreVarExp "eq?", [ CoreNumExp 20.; CoreVarExp "y" ]),
             CoreBoolExp false,
             Flax_core.Constants.void_cond_exp ) ))

let desugar_list_exps _ =
  core_exp_eq "(define x (list))" (CoreAppExp (CoreVarExp "emptylist", []));
  core_exp_eq "(define x (list 1 2))"
    (CoreAppExp
       ( CoreVarExp "cons",
         [
           CoreNumExp 1.0;
           CoreAppExp
             ( CoreVarExp "cons",
               [ CoreNumExp 2.0; CoreAppExp (CoreVarExp "emptylist", []) ] );
         ] ))

let desugar_vector_exps _ =
  core_exp_eq "(define x (vector))"
    (CoreAppExp
       ( CoreLambdaExp
           ([ "$V$" ], CoreBeginExp [ Flax_core.Constants.placeholder_var ], []),
         [ CoreAppExp (CoreVarExp "allocate-array", [ CoreNumExp 0. ]) ] ));
  core_exp_eq "(define x (vector 10 20 30))"
    (CoreAppExp
       ( CoreLambdaExp
           ( [ "$V$" ],
             CoreBeginExp
               [
                 CoreAppExp
                   ( CoreVarExp "array-set!",
                     [
                       Flax_core.Constants.placeholder_var; CoreNumExp 0.; CoreNumExp 10.;
                     ] );
                 CoreAppExp
                   ( CoreVarExp "array-set!",
                     [
                       Flax_core.Constants.placeholder_var; CoreNumExp 1.; CoreNumExp 20.;
                     ] );
                 CoreAppExp
                   ( CoreVarExp "array-set!",
                     [
                       Flax_core.Constants.placeholder_var; CoreNumExp 2.; CoreNumExp 30.;
                     ] );
                 CoreVarExp "$V$";
               ],
             [] ),
         [ CoreAppExp (CoreVarExp "allocate-array", [ CoreNumExp 3. ]) ] ))

let desugar_letrec_exps _ =
  core_exp_eq
    "(define x (letrec ((define (is-even? n) true)   (define (is-odd? n) false)) \
     (is-odd? 11)))"
    (CoreAppExp
       ( CoreLambdaExp
           ( [ "is-even?"; "is-odd?" ],
             CoreBeginExp
               [
                 CoreSetExp ("is-even?", CoreLambdaExp ([ "n" ], CoreBoolExp true, []));
                 CoreSetExp ("is-odd?", CoreLambdaExp ([ "n" ], CoreBoolExp false, []));
                 CoreAppExp (CoreVarExp "is-odd?", [ CoreNumExp 11. ]);
               ],
             [] ),
         [ Flax_core.Constants.dummy_var; Flax_core.Constants.dummy_var ] ));
  core_exp_eq
    "(define x (letrec ((define (is-even? n z) true)   (define (is-odd? n z y) false)) \
     (is-odd? 11 12 13)))"
    (CoreAppExp
       ( CoreLambdaExp
           ( [ "is-even?"; "is-odd?" ],
             CoreBeginExp
               [
                 CoreSetExp
                   ("is-even?", CoreLambdaExp ([ "n"; "z" ], CoreBoolExp true, []));
                 CoreSetExp
                   ("is-odd?", CoreLambdaExp ([ "n"; "z"; "y" ], CoreBoolExp false, []));
                 CoreAppExp
                   ( CoreVarExp "is-odd?",
                     [ CoreNumExp 11.; CoreNumExp 12.; CoreNumExp 13. ] );
               ],
             [] ),
         [ Flax_core.Constants.dummy_var; Flax_core.Constants.dummy_var ] ))

let suite =
  "Desugarar tests"
  >::: [
         "Desugar Defs" >:: desugar_def_exps;
         "Desugar Let" >:: desugar_let_exps;
         "Desugar Cond" >:: desugar_cond_exps;
         "Desugar List" >:: desugar_list_exps;
         "Desugar Vector" >:: desugar_vector_exps;
         "Desugar Letrec" >:: desugar_letrec_exps;
       ]

let _ = run_test_tt_main suite
