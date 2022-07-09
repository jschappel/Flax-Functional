open Grammar.CoreGrammar
open Utils

let freevar_transform_exp _bound_vars _exp = failwith "TODO"

let freevar_transform_def (CoreDef (id, exp)) =
  CoreDef (id, freevar_transform_exp [ id ] exp)

let freevar_transfom_program (CoreProg defs) =
  defs |> List.map freevar_transform_def |> CoreProg


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
