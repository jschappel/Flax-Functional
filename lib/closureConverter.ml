open Flax_grammar.CoreGrammar
open Flax_environment.Env

type core_phase2_prog = core_prog

let prim_env = PrimEnvironment.new_env ()

module EnvNameSymGen = Utils.Generator.SymGen (struct
  let s = "env$"
end)

(* https://matt.might.net/articles/closure-conversion/

   lambda (f): lambda (z): -- generate ref to f lambda (x): -- generate ref to f z (+ f z
   x) *)

module FreeVarMap = Map.Make (String)
module FreeVarSet = Set.Make (String)

(* Finds all free variables in the given expresssion *)
let compute_free_vars exp =
  let rec find_free_vars exp =
    let find_free_vars_lst lst =
      lst |> List.map find_free_vars |> List.fold_left FreeVarSet.union FreeVarSet.empty
    in
    let is_prim v = PrimEnvironment.contains v prim_env in
    match exp with
    | CoreNumExp _ | CoreBoolExp _ | CoreSymExp _ | CoreStrExp _ -> FreeVarSet.empty
    | CoreVarExp v ->
        if not @@ is_prim v then FreeVarSet.of_list [ v ] else FreeVarSet.empty
    | CoreIfExp (e1, e2, e3) ->
        find_free_vars e1
        |> FreeVarSet.union @@ find_free_vars e2
        |> FreeVarSet.union @@ find_free_vars e3
    | CoreLambdaExp (p, b, _) -> FreeVarSet.diff (find_free_vars b) (FreeVarSet.of_list p)
    | CoreOrExp exps -> find_free_vars_lst exps
    | CoreAndExp exps -> find_free_vars_lst exps
    | CoreNotExp e -> find_free_vars e
    | CoreAppExp (r, rands) ->
        FreeVarSet.union (find_free_vars r) (find_free_vars_lst rands)
    | CoreVectorExp exps -> find_free_vars_lst exps
    | CoreListExp exps -> find_free_vars_lst exps
    | CoreSetExp (_, e) -> find_free_vars e
    | CoreBeginExp exps -> find_free_vars_lst exps
    | CorePhase2EnvRefExp e -> find_free_vars e.env
    | CorePhase2MakeEnvExp vals ->
        vals |> List.map (fun (_, e) -> e) |> find_free_vars_lst
    | CorePhase2LambdaExp { params; body } ->
        FreeVarSet.diff (find_free_vars body) (FreeVarSet.of_list params)
    | CorePhase2MakeClosureExp e ->
        FreeVarSet.diff (find_free_vars e.lambda) (find_free_vars e.env)
    | CoreFreeVarExp _ -> failwith "Unreachable"
  in
  find_free_vars exp |> FreeVarSet.to_seq |> List.of_seq

(* Substitutes all free variables into the expression *)
let rec substitute_fvars fvars body =
  let substitute_fvars_in_list exps = List.map (substitute_fvars fvars) exps in
  match body with
  | CoreNumExp _ -> body
  | CoreSymExp _ -> body
  | CoreStrExp _ -> body
  | CoreBoolExp _ -> body
  | CoreVarExp id -> (
      match FreeVarMap.find_opt id fvars with Some v -> v | None -> body)
  | CoreAndExp exps -> CoreAndExp (substitute_fvars_in_list exps)
  | CoreOrExp exps -> CoreOrExp (substitute_fvars_in_list exps)
  | CoreBeginExp exps -> CoreBeginExp (substitute_fvars_in_list exps)
  | CoreVectorExp exps -> CoreVectorExp (substitute_fvars_in_list exps)
  | CoreListExp exps -> CoreListExp (substitute_fvars_in_list exps)
  | CoreNotExp e -> CoreNotExp (substitute_fvars fvars e)
  | CoreIfExp (e1, e2, e3) ->
      CoreIfExp
        (substitute_fvars fvars e1, substitute_fvars fvars e2, substitute_fvars fvars e3)
  | CoreSetExp (i, e) -> CoreSetExp (i, substitute_fvars fvars e)
  | CoreAppExp (rator, rands) ->
      CoreAppExp (substitute_fvars fvars rator, substitute_fvars_in_list rands)
  | CoreFreeVarExp _ -> failwith "TODO REMOVE THIS"
  | CoreLambdaExp (p, b, _) ->
      let non_shadowed_fvars = FreeVarMap.filter (fun k _ -> List.mem k p) fvars in
      CoreLambdaExp (p, substitute_fvars non_shadowed_fvars b, [])
  | CorePhase2LambdaExp l ->
      (*NOTE: there should not be any free variables at this point *)
      let non_shadowed_fvars = FreeVarMap.filter (fun k _ -> List.mem k l.params) fvars in
      CorePhase2LambdaExp { l with body = substitute_fvars non_shadowed_fvars l.body }
  | CorePhase2MakeClosureExp e ->
      CorePhase2MakeClosureExp { e with lambda = substitute_fvars fvars body }
  | CorePhase2EnvRefExp e ->
      CorePhase2EnvRefExp { e with env = substitute_fvars fvars e.env }
  | CorePhase2MakeEnvExp lst ->
      lst
      |> List.map (fun (i, v) -> (i, substitute_fvars fvars v))
      |> CorePhase2MakeEnvExp

let closure_convert_program (CoreProg defs : core_prog) : core_phase2_prog =
  let _global_def_names = List.map (fun (CoreDef (n, _)) -> n) defs in
  let rec closure_convert_def (CoreDef (name, exp)) =
    CoreDef (name, closure_convert_exp exp)
  and closure_convert_exp exp : core_exp =
    let convert_exp_list exps = List.map closure_convert_exp exps in
    match exp with
    | CoreNumExp _ -> exp
    | CoreSymExp _ -> exp
    | CoreStrExp _ -> exp
    | CoreVarExp _ -> exp
    | CoreBoolExp _ -> exp
    | CorePhase2MakeEnvExp _ -> exp
    | CorePhase2MakeClosureExp _ -> exp
    | CorePhase2EnvRefExp _ -> exp
    | CorePhase2LambdaExp _ -> exp
    | CoreAndExp exps -> CoreAndExp (convert_exp_list exps)
    | CoreOrExp exps -> CoreOrExp (convert_exp_list exps)
    | CoreBeginExp exps -> CoreBeginExp (convert_exp_list exps)
    | CoreVectorExp exps -> CoreVectorExp (convert_exp_list exps)
    | CoreListExp exps -> CoreListExp (convert_exp_list exps)
    | CoreNotExp e -> CoreNotExp (closure_convert_exp e)
    | CoreIfExp (e1, e2, e3) ->
        CoreIfExp (closure_convert_exp e1, closure_convert_exp e2, closure_convert_exp e3)
    | CoreSetExp (i, e) -> CoreSetExp (i, closure_convert_exp e)
    | CoreAppExp (rator, rands) ->
        CoreAppExp (closure_convert_exp rator, convert_exp_list rands)
    | CoreLambdaExp (p, b, _) ->
        let env_ref = EnvNameSymGen.gen_sym () in
        let fvars = compute_free_vars exp in
        let fvar_map =
          fvars
          |> List.map (fun id ->
                 (id, CorePhase2EnvRefExp { env = CoreVarExp env_ref; var = id }))
          |> List.to_seq
          |> FreeVarMap.of_seq
        in
        let env = CorePhase2MakeEnvExp (List.map (fun v -> (v, CoreVarExp v)) fvars) in
        let transformed_body = substitute_fvars fvar_map b in
        CorePhase2MakeClosureExp
          {
            lambda =
              CorePhase2LambdaExp { params = [ env_ref ] @ p; body = transformed_body };
            env;
          }
    | CoreFreeVarExp _ -> failwith "TODO REMOVE THIS"
  in
  EnvNameSymGen.reset ();
  defs |> List.map closure_convert_def |> CoreProg
