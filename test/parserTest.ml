open OUnit2
open Flax_core.Lib.Parser
open Flax_core.Lib.Test.Ast

(* Checks if two progams are equivalent to one another *)
let prog_eq (s : string) (v : 'a) : unit =
  assert_equal ~printer:show_program v (parse_program s)

(*
   Checks if two expressions are equivalent to one another, ignoring the outter define expression
   Note: This can only be used to check a program with one definiton
*)
let exp_eq (s : string) (v : 'a) : unit =
  match parse_program s with
  | Program [ Def (_, exp) ] -> assert_equal ~printer:show_exp exp v
  | _ ->
      failwith
        "This Function can only be used to check a program wiht a single \
         definition"

let parse_basic_types _ =
  prog_eq "(define x-y 10)" (Program [ Def ("x-y", NumExp 10.0) ]);
  prog_eq "(define x 10) (define y true)"
    (Program [ Def ("x", NumExp 10.0); Def ("y", BoolExp true) ]);
  prog_eq "(define string \"string\")"
    (Program [ Def ("string", StrExp "string") ]);
  prog_eq "(define sym '10_p )" (Program [ Def ("sym", SymExp "10_p") ]);
  prog_eq "(define x xx)" (Program [ Def ("x", VarExp "xx") ])

let parse_if_exprs _ =
  exp_eq "(define x (if true 1 0))"
    (IfExp (BoolExp true, NumExp 1.0, NumExp 0.0));
  exp_eq "(define x (if true (if false 0 1) 0))"
    (IfExp
       (BoolExp true, IfExp (BoolExp false, NumExp 0.0, NumExp 1.0), NumExp 0.0))

let parse_cond_exprs _ =
  exp_eq "(define x (cond ((eq? x 10) true)   (else false)))"
    (CondExp
       [
         (AppExp (VarExp "eq?", [ VarExp "x"; NumExp 10.0 ]), BoolExp true);
         (SymExp "else", BoolExp false);
       ])

let parse_lambda_exprs _ =
  exp_eq "(define x (lambda () true))" (LambdaExp ([], BoolExp true));
  exp_eq "(define x (lambda (v) v))" (LambdaExp ([ "v" ], VarExp "v"));
  exp_eq "(define x (lambda (x y z) (add x y z)))"
    (LambdaExp
       ( [ "x"; "y"; "z" ],
         AppExp (VarExp "add", [ VarExp "x"; VarExp "y"; VarExp "z" ]) ));
  exp_eq "(define curry-add (lambda (x) (lambda (y) (+ x y))))"
    (LambdaExp
       ( [ "x" ],
         LambdaExp ([ "y" ], AppExp (VarExp "+", [ VarExp "x"; VarExp "y" ])) ))

let parse_let_exprs _ =
  exp_eq "(define x (let ((y 10)) (+ y 9)))"
    (LetExp
       ([ ("y", NumExp 10.0) ], AppExp (VarExp "+", [ VarExp "y"; NumExp 9.0 ])));
  exp_eq "(define x (let ((y 10) (x 30)) (+ y x)))"
    (LetExp
       ( [ ("y", NumExp 10.0); ("x", NumExp 30.0) ],
         AppExp (VarExp "+", [ VarExp "y"; VarExp "x" ]) ));
  exp_eq "(define x (let ((y 10) (x 20)) (let ((z 30)) (+ x y z))))"
    (LetExp
       ( [ ("y", NumExp 10.0); ("x", NumExp 20.0) ],
         LetExp
           ( [ ("z", NumExp 30.0) ],
             AppExp (VarExp "+", [ VarExp "x"; VarExp "y"; VarExp "z" ]) ) ))

let parse_and_exprs _ =
  exp_eq "(define x (and true false true 10))"
    (AndExp [ BoolExp true; BoolExp false; BoolExp true; NumExp 10.0 ]);
  exp_eq "(define x (and true false (and true 10)))"
    (AndExp
       [ BoolExp true; BoolExp false; AndExp [ BoolExp true; NumExp 10.0 ] ])

let parse_or_exprs _ =
  exp_eq "(define x (or true false true 10))"
    (OrExp [ BoolExp true; BoolExp false; BoolExp true; NumExp 10.0 ]);
  exp_eq "(define x (or true false (or true 10)))"
    (OrExp [ BoolExp true; BoolExp false; OrExp [ BoolExp true; NumExp 10.0 ] ])

let parse_not_exprs _ =
  exp_eq "(define x (not true))" (NotExp (BoolExp true));
  exp_eq "(define x (not (lambda (c) true)))"
    (NotExp (LambdaExp ([ "c" ], BoolExp true)))

let parse_app_exprs _ =
  exp_eq "(define x (add-ten x))" (AppExp (VarExp "add-ten", [ VarExp "x" ]));
  exp_eq "(define x (add-ten))" (AppExp (VarExp "add-ten", []));
  exp_eq "(define x (add-ten x y z))"
    (AppExp (VarExp "add-ten", [ VarExp "x"; VarExp "y"; VarExp "z" ]));
  exp_eq "(define x (add-ten x y (add-five xx yy) z))"
    (AppExp
       ( VarExp "add-ten",
         [
           VarExp "x";
           VarExp "y";
           AppExp (VarExp "add-five", [ VarExp "xx"; VarExp "yy" ]);
           VarExp "z";
         ] ))

let parse_vector_exprs _ =
  exp_eq "(define x (vector))" (VectorExp []);
  exp_eq "(define x (vector 1 2 '4 8))"
    (VectorExp [ NumExp 1.; NumExp 2.; SymExp "4"; NumExp 8. ])

let parse_list_exprs _ =
  exp_eq "(define x (list))" (ListExp []);
  exp_eq "(define x (list 1 2 '4 8))"
    (ListExp [ NumExp 1.; NumExp 2.; SymExp "4"; NumExp 8. ])

let parse_set_exprs _ =
  exp_eq "(define x (set! y 10))" (SetExp ("y", NumExp 10.));
  exp_eq "(define x (set! y (+ 4 1)))"
    (SetExp ("y", AppExp (VarExp "+", [ NumExp 4.; NumExp 1. ])))

let parse_begin_exprs _ =
  exp_eq "(define x (begin (set! x 10) (set! y 20)))"
    (BeginExp [ SetExp ("x", NumExp 10.); SetExp ("y", NumExp 20.) ]);
  exp_eq "(define x (begin (list 1 2 '4 8) (+ 2 1)))"
    (BeginExp
       [
         ListExp [ NumExp 1.; NumExp 2.; SymExp "4"; NumExp 8. ];
         AppExp (VarExp "+", [ NumExp 2.; NumExp 1. ]);
       ])

let parse_func_defs _ = 
  prog_eq "(define (add) (+))"
  (Program [DefFunc("add", [], AppExp(VarExp "+", []))]);

  prog_eq "(define (sub x y) (- x y))"
  (Program [DefFunc("sub", ["x";"y"], AppExp(VarExp"-", [VarExp "x";VarExp"y"]))])



let suite =
  "Parser tests"
  >::: [
         "Basic Types" >:: parse_basic_types;
         "If Expressions" >:: parse_if_exprs;
         "Cond Expressions" >:: parse_cond_exprs;
         "Lambda Expressions" >:: parse_lambda_exprs;
         "Let Expressions" >:: parse_let_exprs;
         "And Expressions" >:: parse_and_exprs;
         "Or Expressions" >:: parse_or_exprs;
         "Not Expressions" >:: parse_not_exprs;
         "Application Expressions" >:: parse_app_exprs;
         "Vector Expressions" >:: parse_vector_exprs;
         "List Expressions" >:: parse_list_exprs;
         "Set Expressions" >:: parse_set_exprs;
         "Begin Expressions" >:: parse_begin_exprs;
         "Define Functions" >:: parse_func_defs;
       ]

let _ = run_test_tt_main suite
