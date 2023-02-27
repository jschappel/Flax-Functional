open Flax_grammar.CoreGrammar
open Flax_environment.Env

type core_phase2_prog = core_prog


let prim_env = PrimEnvironment.new_env ()

module EnvNameSymGen = Utils.Generator.SymGen (struct
  let s = "env$"
end)

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
  | CoreAppExp (r, rands) -> ListUtils.ormap (occurs_free var) (rands @ [ r ])
  | CoreVectorExp exps -> ListUtils.ormap (occurs_free var) exps
  | CoreListExp exps -> ListUtils.ormap (occurs_free var) exps
  | CoreSetExp (_, e) -> occurs_free var e
  | CoreBeginExp exps -> ListUtils.ormap (occurs_free var) exps
  | CorePhase2RefExp {id; _} -> String.equal id var
  | CorePhase2ClosureExp { body; env; _} -> 
    let env_vars = List.map (fun (x, _) -> x) env in
    (not @@ List.mem var env_vars) && occurs_free var body
  | CoreFreeVarExp _ -> failwith "Unreachable"

let rec substitute_fvars env_ref fvars body =
  let substitute_fvars_in_list exps = List.map (substitute_fvars env_ref fvars) exps in
  match body with
| CoreNumExp n -> CoreNumExp n
| CoreSymExp s -> CoreSymExp s
| CoreStrExp s -> CoreStrExp s
| CorePhase2RefExp s -> CorePhase2RefExp s
| CoreVarExp id when List.mem id fvars-> CorePhase2RefExp {env_ref; id }
| CoreVarExp v -> CoreVarExp v
| CoreBoolExp b -> CoreBoolExp b
| CoreAndExp exps -> CoreAndExp (substitute_fvars_in_list exps)
| CoreOrExp exps -> CoreOrExp (substitute_fvars_in_list exps)
| CoreBeginExp exps -> CoreBeginExp (substitute_fvars_in_list exps)
| CoreVectorExp exps -> CoreVectorExp (substitute_fvars_in_list exps)
| CoreListExp exps -> CoreListExp (substitute_fvars_in_list exps)
| CoreNotExp e -> CoreNotExp (substitute_fvars env_ref fvars e)
| CoreIfExp (e1, e2, e3) ->
  CoreIfExp (substitute_fvars env_ref fvars e1, substitute_fvars env_ref fvars e2, substitute_fvars env_ref fvars e3)
| CoreSetExp (i, e) -> CoreSetExp (i, substitute_fvars env_ref fvars e)
| CoreAppExp (rator, rands) ->
  CoreAppExp (substitute_fvars env_ref fvars rator, substitute_fvars_in_list rands)
| CoreFreeVarExp _ -> failwith "TODO REMOVE THIS"
| CoreLambdaExp (p, b, fvars) -> failwith "TODO"

let closure_convert_program (CoreProg defs : core_prog): core_phase2_prog =
  let global_def_names = List.map (fun (CoreDef (n, _)) -> n) defs in
  let is_prim v = PrimEnvironment.contains v prim_env in

  (* Closure converts a core def *)
  let rec closure_convert_def (CoreDef (name, exp)) =
    CoreDef (name, closure_convert_exp exp)
  (* Closure converts a core exp *)
  and closure_convert_exp exp : core_exp =
    let convert_exp_list exps = List.map closure_convert_exp exps in
    match exp with
    | CoreNumExp n -> CoreNumExp n
    | CoreSymExp s -> CoreSymExp s
    | CoreStrExp s -> CoreStrExp s
    | CoreVarExp v -> CoreVarExp v
    | CoreBoolExp b -> CoreBoolExp b
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
    | CoreLambdaExp (p, b, fvars) ->
        let env_ref = EnvNameSymGen.gen_sym () in
        let fvars = List.filter (fun v -> occurs_free v b) fvars in
        let transformed_body = substitute_fvars env_ref fvars  b in
        CorePhase2ClosureExp
          {
            body = CoreLambdaExp (p, transformed_body, []);
            ref_id = env_ref;
            env = List.map (fun s -> (s, Var s)) fvars;
          }
    | CoreFreeVarExp _ -> failwith "TODO REMOVE THIS"
    | CorePhase2RefExp _ -> exp
    | CorePhase2ClosureExp _ -> exp
  in
  let prog = defs |> List.map closure_convert_def |> CoreProg in
  EnvNameSymGen.reset ();
  prog


  (*

  https://matt.might.net/articles/closure-conversion/
  
  
  
  lambda (f):
    lambda (z):          -- generate ref to f
      lambda (x):        -- generate ref to f z
        (+ f z x)   
  
  
  
  *)