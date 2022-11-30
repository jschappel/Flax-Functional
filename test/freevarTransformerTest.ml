open OUnit2
open Flax_core.Lib.Grammar.CoreGrammar
(* open Flax_core.FreevarTransformer *)

(* returns a list of all the free variables in the program *)
let find_all_free_vars (prog : string) : string list =
  let rec find_all_free exp =
    let find_all_free_list exps = List.map find_all_free exps |> List.flatten in
    match exp with
    | CoreNumExp _ -> []
    | CoreSymExp _ -> []
    | CoreStrExp _ -> []
    | CoreBoolExp _ -> []
    | CoreVarExp _ -> []
    | CoreFreeVarExp _ -> []
    | CoreIfExp (e1, e2, e3) -> find_all_free e1 @ find_all_free e2 @ find_all_free e3
    | CoreNotExp e -> find_all_free e
    | CoreOrExp exps -> find_all_free_list exps
    | CoreAndExp exps -> find_all_free_list exps
    | CoreBeginExp exps -> find_all_free_list exps
    | CoreListExp exps -> find_all_free_list exps
    | CoreVectorExp exps -> find_all_free_list exps
    | CoreSetExp (_, exp) -> find_all_free exp
    | CoreAppExp (rator, rands) -> find_all_free rator @ find_all_free_list rands
    | CoreLambdaExp (_, b, fvars) -> find_all_free b @ fvars
  in
  let transform_single_prog single_def =
    let open Flax_core.Lib in
    let (CoreProg defs) =
      single_def
      |> Parser.parse_program
      |> Desugarar.desugar_program
      (* |> Cps.cps_program *)
      |> FreevarTransformer.freevar_transfom_program
    in
    List.map (fun (CoreDef (_, exp)) -> find_all_free exp) defs
    |> List.flatten
    |> List.sort_uniq String.compare
  in
  transform_single_prog prog

let fvar_transform_basics _ =
  assert_equal ~printer:[%derive.show: string list] []
    (find_all_free_vars "(define x 10)");
  assert_equal ~printer:[%derive.show: string list] []
    (find_all_free_vars "(define x false)");
  assert_equal ~printer:[%derive.show: string list] []
    (find_all_free_vars "(define x 'hello )");
  assert_equal ~printer:[%derive.show: string list] []
    (find_all_free_vars "(define x \"Hello world\")");
  assert_equal ~printer:[%derive.show: string list] []
    (find_all_free_vars "(define y 10) (define x y)");
  assert_equal ~printer:[%derive.show: string list] [ "zz" ]
    (find_all_free_vars
       "(define x 10) (define dummy (if (equal? x 0) 1 (lambda (y z) (+ y z zz))))")

let fvar_transform_nested _ =
  assert_equal ~printer:[%derive.show: string list] [ "a"; "b" ]
    (find_all_free_vars
       "(define test
          (lambda (a b)
            (lambda (c d) 
              (+ a b c d))))
        ");

  assert_equal ~printer:[%derive.show: string list] [ "a"; "b"; "c"; "d" ]
    (find_all_free_vars
       "(define test
          (lambda (a b)
            (lambda (c d)
              (lambda (e f)
              (+ a b c d e f)))))
        ");
  assert_equal ~printer:[%derive.show: string list] [ "a"; "b"; "c"; "d" ]
    (find_all_free_vars
       "(define AA 10)
        (define test
          (lambda (a b)
            (lambda (c d)
              (lambda (e f)
                (+ a b c d e f AA)))))
        ")

  
let fvar_transform_shadowing _ =
  assert_equal ~printer:[%derive.show: string list] ["b"]
    (find_all_free_vars
      "(define test
          (lambda (a b)
            (lambda (a d)
              (+ a b d))))
        ");
  assert_equal ~printer:[%derive.show: string list] [ "a"; "b" ;"c"; "d"]
    (find_all_free_vars
      "(define test
          (lambda (a b)
            (lambda (c d)
              (begin 
                (+ a b c d)
                (lambda (a) (+ a b c d))))))
        ")

let suite =
  "Freevar Transformer tests"
  >::: [
         (* "Freevar Transform Basic expressions" >:: fvar_transform_basics ;
         "Freevar Transform Nested Lambdas" >:: fvar_transform_nested; *)
         "Freevar Transform Shadowed Lambdas" >:: fvar_transform_shadowing;
       ]

let _ = run_test_tt_main suite
