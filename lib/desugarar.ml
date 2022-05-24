open Flax_parser.Ast
open CoreGrammer
open Utils

let rec desugar_exp = function
  | NumExp n -> CoreNumExp n
  | BoolExp b -> CoreBoolExp b
  | SymExp s -> CoreSymExp s
  | StrExp s -> CoreStrExp s
  | VarExp v -> CoreVarExp v
  | IfExp (e1, e2, e3) ->
      CoreIfExp (desugar_exp e1, desugar_exp e2, desugar_exp e3)
  | LambdaExp (ids, e) -> CoreLambdaExp (ids, desugar_exp e, [])
  | AndExp lst -> List.map desugar_exp lst |> CoreAndExp
  | OrExp lst -> List.map desugar_exp lst |> CoreOfExp
  | NotExp e -> desugar_exp e |> CoreNotExp
  | AppExp (e, lst) -> CoreAppExp (desugar_exp e, List.map desugar_exp lst)
  | SetExp (id, e) -> CoreSetExp (id, desugar_exp e)
  | BeginExp lst -> List.map desugar_exp lst |> CoreBeginExp
  | CondExp lst -> desugar_cond lst
  | LetExp (lst, e) -> desugar_let e lst
  | ListExp lst -> desugar_list lst
  | VectorExp lst -> desugar_vector lst
  | _ -> failwith "error"

(* If the source expression is a listexp, create a core-listexp
   by desugaring all of the expressions in the listexp.
*)
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
                    Constants.placeholder_var;
                    Int.to_float i |> CoreNumExp;
                    desugar_exp e;
                  ] ))
            lst)
    |> CoreBeginExp
  in
  CoreAppExp
    ( CoreLambdaExp ([ "$V$" ], begin_exp, []),
      [
        CoreAppExp
          ( CoreVarExp "allocate-array",
            [ List.length lst |> Int.to_float |> CoreNumExp ] );
      ] )

(*
  create a core-appexp with the operator being a core-lambdaexp where the same 
  parameters are the variables of the let, and the body of the lambda being
  the desugared body of the source let, while the operands of the core-appexp
  are the desugared right hand side expressions of the source letexp.
*)
and desugar_let e lst =
  let params = List.map (fun (v, _) -> v) lst in
  let exps = List.map (fun (_, e) -> desugar_exp e) lst in
  let body = desugar_exp e in
  CoreAppExp (CoreLambdaExp (params, body, []), exps)

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
