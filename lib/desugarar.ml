open Flax_parser.Ast
open CoreGrammer

(* A detailed explination of how each of the expression are desugared can be found in:
   `doc/desugar.md` *)

let rec desugar_exp = function
  | NumExp n -> CoreNumExp n
  | BoolExp b -> CoreBoolExp b
  | SymExp s -> CoreSymExp s
  | StrExp s -> CoreStrExp s
  | VarExp v -> CoreVarExp v
  | IfExp (e1, e2, e3) -> CoreIfExp (desugar_exp e1, desugar_exp e2, desugar_exp e3)
  | LambdaExp (ids, e) -> CoreLambdaExp (ids, desugar_exp e, [])
  | AndExp lst -> List.map desugar_exp lst |> CoreAndExp
  | OrExp lst -> List.map desugar_exp lst |> CoreOrExp
  | NotExp e -> desugar_exp e |> CoreNotExp
  | AppExp (e, lst) -> CoreAppExp (desugar_exp e, List.map desugar_exp lst)
  | SetExp (id, e) -> CoreSetExp (id, desugar_exp e)
  | BeginExp lst -> List.map desugar_exp lst |> CoreBeginExp
  | CondExp lst -> desugar_cond lst
  | LetExp (lst, e) -> desugar_let e lst
  | ListExp lst -> desugar_list lst
  | VectorExp lst -> desugar_vector lst
  | LetRecExp (defs, e) -> desugar_letrec defs e

and desugar_list lst =
  CoreAppExp (CoreVarExp "emptylist", [])
  |> List.fold_right
       (fun e acc -> CoreAppExp (CoreVarExp "cons", [ desugar_exp e; acc ]))
       lst

and desugar_vector lst =
  let begin_exp =
    [ Constants.placeholder_var ]
    |> List.append
         (List.mapi
            (fun i e ->
              CoreAppExp
                ( CoreVarExp "array-set!",
                  [
                    Constants.placeholder_var; Int.to_float i |> CoreNumExp; desugar_exp e;
                  ] ))
            lst)
    |> CoreBeginExp
  in
  CoreAppExp
    ( CoreLambdaExp ([ "$V$" ], begin_exp, []),
      [
        CoreAppExp
          (CoreVarExp "allocate-array", [ List.length lst |> Int.to_float |> CoreNumExp ]);
      ] )

and desugar_let e lst =
  let params = List.map (fun (v, _) -> v) lst in
  let exps = List.map (fun (_, e) -> desugar_exp e) lst in
  let body = desugar_exp e in
  CoreAppExp (CoreLambdaExp (params, body, []), exps)

and desugar_letrec defs e =
  let dummies _ = Constants.dummy_var in
  let params = List.map (fun (n, _, _) -> n) defs in
  let mk_set_exp (n, p, e) = CoreSetExp (n, CoreLambdaExp (p, desugar_exp e, [])) in
  let set_exps =
    [ desugar_exp e ] |> List.append (List.map mk_set_exp defs) |> CoreBeginExp
  in
  CoreAppExp (CoreLambdaExp (params, set_exps, []), List.map dummies params)

and desugar_cond lst =
  let rec helper = function
    | [] -> Constants.void_cond_exp
    | [ (SymExp "else", r) ] -> desugar_exp r
    | (l, r) :: xs -> CoreIfExp (desugar_exp l, desugar_exp r, helper xs)
  in
  match lst with
  | [ (SymExp "else", r) ] ->
      CoreIfExp (CoreBoolExp true, desugar_exp r, Constants.void_cond_exp)
  | l -> helper l

let desugar_def = function
  | Def (n, e) -> CoreDef (n, desugar_exp e)
  | DefFunc (n, ids, e) -> CoreDef (n, CoreLambdaExp (ids, desugar_exp e, []))

let desugar_program (Program defs) = List.map desugar_def defs |> CoreProg
