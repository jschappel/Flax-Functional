open OUnit2
open Flax_core.Cps
open Flax_core.CoreGrammer

(* Makes sure that the two programs are equivlent *)
let rec assert_prog_eq (actual : string) (CoreProg expected) : unit =
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
  assert_equal (SymGen.gen_sym ()) "v1";
  assert_equal (SymGen.gen_sym ()) "v2";
  assert_equal (SymGen.reset () |> SymGen.gen_sym) "v1"

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
               ([ "x"; "$k$" ], CoreAppExp (CoreVarExp "$k$", [ CoreNumExp 3.0 ]), []) );
       ])

let suite =
  "Cps tests" >::: [ "SymGen" >:: sym_gen_test; "Cps Basic Function1" >:: cps_function_1 ]

let _ = run_test_tt_main suite
