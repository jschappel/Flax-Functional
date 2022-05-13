open OUnit2
open Flax_core.Lib.Parser
open Flax_core.Lib.Test.Ast

let prog_eq (s : string) (v : 'a) = assert_equal v (parse_program s)

let parse_basic_types _ =
  prog_eq "(define x 10)" (Program [ Def ("x", NumExp 10.0) ]);
  prog_eq "(define x 10) (define y true)"
    (Program [ Def ("x", NumExp 10.0); Def ("y", BoolExp true) ]);
  prog_eq "(define string \"string\")"
    (Program [ Def ("string", StrExp "string") ]);
  prog_eq "(define sym '10_p )" (Program [ Def ("sym", SymExp "10_p") ]);
  prog_eq "(define x xx)" (Program [ Def ("x", VarExp "xx") ])

let parse_if_exprs _ =
  prog_eq "(define x (if true 1 0))"
    (Program [ Def ("x", IfExp (BoolExp true, NumExp 1.0, NumExp 0.0)) ]);
  prog_eq "(define x (if true (if false 0 1) 0))"
    (Program
       [
         Def
           ( "x",
             IfExp
               ( BoolExp true,
                 IfExp (BoolExp false, NumExp 0.0, NumExp 1.0),
                 NumExp 0.0 ) );
       ])

let parse_cond_exprs _ =
  prog_eq "(define x (cond ((eq? x 10) true)   (else false)))"
    (Program
       [
         Def
           ( "x",
             CondExp
               [
                 ( AppExp (VarExp "eq?", [ VarExp "x"; BoolExp true ]),
                   BoolExp true );
                 (VarExp "else", BoolExp false);
               ] );
       ])

let suite =
  "Parser tests"
  >::: [
         "Basic Types" >:: parse_basic_types;
         "If Expressions" >:: parse_if_exprs;
       ]

let _ = run_test_tt_main suite
