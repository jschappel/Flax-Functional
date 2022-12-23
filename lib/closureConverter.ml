open Flax_grammar.CoreGrammar
open Flax_grammar.CoreGrammar2
open Flax_environment.Env

let prim_env = PrimEnvironment.new_env ()

module EnvNameSymGen = Utils.Generator.SymGen (struct
  let s = "env$"
end)

let substitute_fvars fvars body : core_exp_phase2 = assert false

let generate_env_make fvars env : core_exp_phase2 = assert false

let closure_convert_program (CoreProg defs : core_prog) =
  let global_def_names = List.map (fun (CoreDef (n, _)) -> n) defs in
  let is_prim v = PrimEnvironment.contains v prim_env in

  (* Closure converts a core def *)
  let rec closure_convert_def (CoreDef (name, exp)) =
    Core2Def (name, closure_convert_exp [ name ] exp)
  (* Closure converts a core exp *)
  and closure_convert_exp bound_vars exp : core_exp_phase2 =
    let convert_exp_list exps = List.map (closure_convert_exp bound_vars) exps in
    match exp with
    | CoreNumExp n -> Core2NumExp n
    | CoreSymExp s -> Core2SymExp s
    | CoreStrExp s -> Core2StrExp s
    | CoreVarExp v -> Core2VarExp v
    | CoreBoolExp b -> Core2BoolExp b
    | CoreAndExp exps -> Core2AndExp (convert_exp_list exps)
    | CoreOrExp exps -> Core2OrExp (convert_exp_list exps)
    | CoreBeginExp exps -> Core2BeginExp (convert_exp_list exps)
    | CoreVectorExp exps -> Core2VectorExp (convert_exp_list exps)
    | CoreListExp exps -> Core2ListExp (convert_exp_list exps)
    | CoreNotExp e -> Core2NotExp (closure_convert_exp bound_vars e)
    | CoreIfExp (e1, e2, e3) ->
        Core2IfExp
          ( closure_convert_exp bound_vars e1,
            closure_convert_exp bound_vars e2,
            closure_convert_exp bound_vars e3 )
    | CoreSetExp (i, e) -> Core2SetExp (i, closure_convert_exp bound_vars e)
    | CoreAppExp (rator, rands) ->
        Core2AppExp (closure_convert_exp bound_vars rator, convert_exp_list rands)
    | CoreLambdaExp (p, b, _) ->
        let env = EnvNameSymGen.gen_sym () in
        let fvars = compute_fvars (bound_vars @ p) b in
        let transformed_body = substitute_fvars fvars b in
        let make_env = generate_env_make fvars env in
        Core2ClosureExp { body = transformed_body; env = make_env }
    | CoreFreeVarExp _ -> failwith "TODO REMOVE THIS"
  (* Finds all the free variables in the given expression *)
  and compute_fvars bound_vars exp =
    let compute_fvars_list exps =
      exps
      |> List.fold_left (fun a e -> compute_fvars bound_vars e @ a) []
      |> List.sort_uniq String.compare
    in
    match exp with
    | CoreNumExp _ -> []
    | CoreSymExp _ -> []
    | CoreStrExp _ -> []
    | CoreBoolExp _ -> []
    | CoreVarExp v ->
        if List.mem v bound_vars || List.mem v global_def_names || is_prim v then []
        else [ v ]
    | CoreAndExp exps -> compute_fvars_list exps
    | CoreOrExp exps -> compute_fvars_list exps
    | CoreBeginExp exps -> compute_fvars_list exps
    | CoreVectorExp exps -> compute_fvars_list exps
    | CoreListExp exps -> compute_fvars_list exps
    | CoreNotExp e -> compute_fvars bound_vars e
    | CoreIfExp (e1, e2, e3) -> compute_fvars_list [ e1; e2; e3 ]
    | CoreAppExp (rator, rands) -> compute_fvars_list @@ (rator :: rands)
    | CoreLambdaExp (p, b, _) -> compute_fvars (bound_vars @ p) b
    | CoreSetExp (_, e) -> compute_fvars bound_vars e
    | CoreFreeVarExp _ -> failwith "TODO REMOVE THIS"
  in
  let prog = defs |> List.map closure_convert_def |> Core2Prog in
  EnvNameSymGen.reset ();
  prog
