open Flax_grammar.CoreGrammar
open Flax_environment.Env

module FreeVarEnvironment = AlphaEnvironment

let rec freevar_transform_exp bound_vars exp = match exp with
| CoreNumExp _  -> exp
| CoreBoolExp _ -> exp 
| CoreSymExp _ -> exp
| CoreStrExp _ -> exp
| CoreVarExp _ -> exp (*TODO: Double check this case *)
| CoreIfExp(e1,e2,e3) -> CoreIfExp(freevar_transform_exp bound_vars e1, freevar_transform_exp bound_vars e2, freevar_transform_exp bound_vars e3)
(* | CoreLambdaExp(p, b, f) -> 
  let new_bound_vars = p @ bound_vars in
  CoreLambdaExp *)
| _ -> failwith "TODO"

let freevar_transform_def bound_vars (CoreDef (id, exp)) =
  CoreDef (id, freevar_transform_exp bound_vars exp)

let freevar_transfom_program (CoreProg defs) =
  let bound_vars = List.map (fun (CoreDef(n, _)) -> n) defs in
  defs |> List.map (freevar_transform_def bound_vars) |> CoreProg

let rec occurs_free var exp =
  let open Utils in
  match exp with
  | CoreNumExp _ | CoreBoolExp _ | CoreSymExp _ | CoreStrExp _ -> false
  | CoreVarExp v -> String.equal v var
  | CoreIfExp (e1, e2, e3) ->
      occurs_free var e1 || occurs_free var e2 || occurs_free var e3
  | CoreLambdaExp (p, b, _) -> (not @@ List.mem var p) && occurs_free var b
  | CoreOrExp exps -> ListUtils.ormap (occurs_free var) exps
  | CoreAndExp exps -> ListUtils.ormap (occurs_free var) exps
  | CoreNotExp e -> occurs_free var e
  | CoreAppExp (r, rands) ->
      [ r ] |> List.append rands |> ListUtils.ormap (occurs_free var)
  | CoreVectorExp exps -> ListUtils.ormap (occurs_free var) exps
  | CoreListExp exps -> ListUtils.ormap (occurs_free var) exps
  | CoreSetExp (_, e) -> occurs_free var e
  | CoreBeginExp exps -> ListUtils.ormap (occurs_free var) exps
  | CoreFreeVarExp _ -> failwith "Unreachable"
