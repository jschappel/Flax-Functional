open Flax_parser.Ast
open CoreGrammer
open Utils

let rec desugar_exp = function 
| NumExp n -> CoreNumExp n
| BoolExp b  -> CoreBoolExp b
| SymExp s -> CoreSymExp s
| StrExp s -> CoreStrExp s
| VarExp v -> CoreVarExp v
| IfExp(e1, e2, e3) -> CoreIfExp(desugar_exp e1, desugar_exp e2, desugar_exp e3)
| LambdaExp(ids, e) -> CoreLambdaExp(ids, desugar_exp e, [])
| AndExp(lst) -> List.map desugar_exp lst |> CoreAndExp
| OrExp(lst) -> List.map desugar_exp lst |> CoreOfExp
| NotExp(e) -> desugar_exp e |> CoreNotExp
| AppExp(e, lst) -> CoreAppExp(desugar_exp e, List.map desugar_exp lst)
| VectorExp(lst) -> List.map desugar_exp lst |> CoreVectorExp
| ListExp(lst) -> List.map desugar_exp lst |> CoreListExp
| SetExp(id, e) -> CoreSetExp(id, desugar_exp e)
| BeginExp(lst) -> List.map desugar_exp lst |> CoreBeginExp
| CondExp(lst) -> desugar_cond lst
| _ -> failwith "Error"
and desugar_cond lst =
  let rec helper = function
  | [(VarExp "else", r)] -> desugar_exp r
  | (l, r) :: xs -> CoreIfExp(desugar_exp l, desugar_exp r, helper xs)
  | _ -> Branch.unreachable ()
in
match lst with
| [(VarExp "else", r)] -> desugar_exp r
| l -> helper l
| _ -> failwith "Expected at least two cond branchs, or one else branch"

let desugar_def = function
| Def(n, e) -> CoreDef(n, desugar_exp e)
| DefFunc(n, ids, e) -> CoreDef(n, CoreLambdaExp(ids, desugar_exp e, []))

let desugar_program (Program defs) = List.map desugar_def defs |> CoreProg
