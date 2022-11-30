open Flax_grammar.CoreGrammar
open Flax_environment.Env

let prim_env = PrimEnvironment.new_env ()

let freevar_transfom_program (CoreProg defs : core_prog) =
  let global_def_names = List.map (fun (CoreDef (n, _)) -> n) defs in

  (* Computes the free variables for a core def *)
  let rec freevar_transform_def (CoreDef (name, exp)) =
    CoreDef (name, freevar_transform_exp [ name ] exp)
  (* Computes the free variables for a core exp *)
  and freevar_transform_exp bound_vars exp : core_exp =
    let is_prim v = PrimEnvironment.contains v prim_env in

    (* converts a list of core_exps to a tuple containing the computed fvar expresssions
       and a list contained the computed fvars for all the fvar expressions *)
    let rec helper bound_vars fvars exp : core_exp * string list =
      let transform_list exps =
        let results = List.map (helper bound_vars fvars) exps in
        ( List.map (fun (x, _) -> x) results,
          List.fold_left (fun acc (_, f) -> f @ acc) [] results )
      in
      match exp with
      | CoreNumExp _ -> (exp, fvars)
      | CoreBoolExp _ -> (exp, fvars)
      | CoreSymExp _ -> (exp, fvars)
      | CoreStrExp _ -> (exp, fvars)
      (* If the variable is not a bound variable, not a global definition, and not a
         primitive variable then it is free *)
      | CoreVarExp v ->
          if List.mem v bound_vars || List.mem v global_def_names || is_prim v then
            (exp, fvars)
          else (CoreFreeVarExp (v, 0), v :: fvars)
      | CoreFreeVarExp _ -> (exp, fvars)
      | CoreIfExp (e1, e2, e3) ->
          let f_exp1, computed_fvars1 = helper bound_vars fvars e1 in
          let f_exp2, computed_fvars2 = helper bound_vars fvars e2 in
          let f_exp3, computed_fvars3 = helper bound_vars fvars e3 in
          ( CoreIfExp (f_exp1, f_exp2, f_exp3),
            computed_fvars1 @ computed_fvars2 @ computed_fvars3 )
      | CoreNotExp e ->
          let f_exp, computed_fvars = helper bound_vars fvars e in
          (CoreNotExp f_exp, computed_fvars)
      | CoreOrExp exps ->
          let f_exps, computed_fvars = transform_list exps in
          (CoreOrExp f_exps, computed_fvars)
      | CoreAndExp exps ->
          let f_exps, computed_fvars = transform_list exps in
          (CoreAndExp f_exps, computed_fvars)
      | CoreBeginExp exps ->
          let f_exps, computed_fvars = transform_list exps in
          (CoreBeginExp f_exps, computed_fvars)
      | CoreListExp exps ->
          let f_exps, computed_fvars = transform_list exps in
          (CoreListExp f_exps, computed_fvars)
      | CoreVectorExp exps ->
          let f_exps, computed_fvars = transform_list exps in
          (CoreVectorExp f_exps, computed_fvars)
      | CoreSetExp (v, e1) ->
          let f_exp1, computed_fvars = helper bound_vars fvars e1 in
          (CoreSetExp (v, f_exp1), computed_fvars)
      | CoreAppExp (rator, rands) ->
          let f_rator, computed_fvars_rator = helper bound_vars fvars rator in
          let f_rands, computed_fvars_rands = transform_list rands in
          (CoreAppExp (f_rator, f_rands), computed_fvars_rator @ computed_fvars_rands)
      | CoreLambdaExp (params, body, fvars) ->
          let new_bound_vars = bound_vars @ params in
          let transformed_body, computed_fvars = helper new_bound_vars fvars body in
          ( CoreLambdaExp (params, transformed_body, computed_fvars),
            List.filter (fun v -> List.mem v params) fvars )
    in
    let transformed_exp, fvars = helper bound_vars [] exp in
    if fvars = [] then transformed_exp
    else
      failwith
      @@ Printf.sprintf
           "Internal Freevar Transformer Compiler Error. Expected empty but was given: %s"
      @@ [%show: string list] fvars
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
