open Flax_grammar.CoreGrammar
open Flax_environment.Env

let prim_env = PrimEnvironment.new_env ()

let compute_fvars bound_vars exp : string list =
  let is_prim v = PrimEnvironment.contains v prim_env in
  match exp with
  | CoreNumExp _ | CoreBoolExp _ | CoreSymExp _ | CoreStrExp _ -> []
  (* If the variable is not a bound variable and not a primitive variable then it is free *)
  | CoreVarExp v -> if List.mem v bound_vars || is_prim v then [] else [ v ]
  | _ -> failwith "TODO"

let substitue_fvars fvars exp : core_exp = failwith "TODO"

let freevar_transfom_program (CoreProg defs : core_prog) =
  let global_def_names = List.map (fun (CoreDef (n, _)) -> n) defs in

  (* Computes the free variables for a core def *)
  let rec freevar_transform_def (CoreDef (name, exp)) =
    CoreDef (name, freevar_transform_exp [ name ] exp)
  (* Computes the free variables for a core exp *)
  and freevar_transform_exp bound_vars exp =
    match exp with
    | CoreNumExp _ -> exp
    | CoreBoolExp _ -> exp
    | CoreSymExp _ -> exp
    | CoreStrExp _ -> exp
    | CoreVarExp _ -> exp
    | CoreFreeVarExp _ -> exp
    | CoreIfExp (e1, e2, e3) ->
        CoreIfExp
          ( freevar_transform_exp bound_vars e1,
            freevar_transform_exp bound_vars e2,
            freevar_transform_exp bound_vars e3 )
    | CoreNotExp e -> freevar_transform_exp bound_vars e |> CoreNotExp
    | CoreOrExp exps -> List.map (freevar_transform_exp bound_vars) exps |> CoreOrExp
    | CoreAndExp exps -> List.map (freevar_transform_exp bound_vars) exps |> CoreAndExp
    | CoreBeginExp exps ->
        List.map (freevar_transform_exp bound_vars) exps |> CoreBeginExp
    | CoreListExp exps -> List.map (freevar_transform_exp bound_vars) exps |> CoreListExp
    | CoreVectorExp exps ->
        List.map (freevar_transform_exp bound_vars) exps |> CoreVectorExp
    | CoreSetExp (v, e) -> CoreSetExp (v, freevar_transform_exp bound_vars e)
    | CoreAppExp (rator, rands) ->
        CoreAppExp
          ( freevar_transform_exp bound_vars rator,
            List.map (freevar_transform_exp bound_vars) rands )
    | CoreLambdaExp (params, body, fvars) ->
        let new_bound_vars = bound_vars @ params in
        let new_fvars =
          List.filter
            (fun v -> not (List.mem v global_def_names))
            (fvars @ compute_fvars new_bound_vars body)
        in
        let new_body =
          substitue_fvars new_fvars (freevar_transform_exp new_bound_vars body)
        in
        CoreLambdaExp (params, new_body, new_fvars)
  in

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
