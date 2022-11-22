open Flax_grammar.CoreGrammar
open Flax_environment.Env
open Utils.Generator

(* Unique symbol generator for definitons *)
module AlphaDefGen = SymGenFmt (struct
  let fmt num = "__D" ^ Int.to_string num ^ "__"
end)

(* Unique symbol generator for variables *)
module AlphaVarGen = SymGenFmt (struct
  let fmt num = "__V" ^ Int.to_string num ^ "__"
end)

(* Unique symbol generator for continuation var `$k$` *)
module AlphaContGen = SymGenFmt (struct
  let fmt num = "__K" ^ Int.to_string num ^ "__"
end)

let prim_env = PrimEnvironment.new_env ()

let rec alpha_convert_exp env exp =
  let get_alpha_var var =
    match AlphaEnvironment.apply var env with
    | Some v -> v
    | None ->
        if String.equal var "end-k" then var
        else failwith @@ "No alphaname generated for variable: " ^ var
  in
  match exp with
  | CoreNumExp _ | CoreSymExp _ | CoreStrExp _ | CoreBoolExp _ -> exp
  | CoreVarExp v -> get_alpha_var v |> CoreVarExp
  | CoreIfExp (e1, e2, e3) ->
      CoreIfExp
        (alpha_convert_exp env e1, alpha_convert_exp env e2, alpha_convert_exp env e3)
  | CoreAndExp exps -> List.map (alpha_convert_exp env) exps |> CoreAndExp
  | CoreOrExp exps -> List.map (alpha_convert_exp env) exps |> CoreOrExp
  | CoreVectorExp exps -> List.map (alpha_convert_exp env) exps |> CoreVectorExp
  | CoreListExp exps -> List.map (alpha_convert_exp env) exps |> CoreListExp
  | CoreBeginExp exps -> List.map (alpha_convert_exp env) exps |> CoreBeginExp
  | CoreNotExp e1 -> alpha_convert_exp env e1 |> CoreNotExp
  | CoreLambdaExp (params, body, fvars) ->
      let alpha_params =
        List.map
          (fun v ->
            match v with
            | "$k$" -> (v, AlphaContGen.gen_sym ())
            | _ -> (v, AlphaVarGen.gen_sym ()))
          params
      in
      let new_env =
        List.fold_left
          (fun env (v, a) -> AlphaEnvironment.add v a env)
          (AlphaEnvironment.new_scope env)
          alpha_params
      in
      CoreLambdaExp
        (List.map (fun (_, a) -> a) alpha_params, alpha_convert_exp new_env body, fvars)
  | CoreAppExp ((CoreVarExp rator as r), rands)
    when PrimEnvironment.contains rator prim_env ->
      CoreAppExp (r, List.map (alpha_convert_exp env) rands)
  | CoreAppExp(rator, rands) -> CoreAppExp(alpha_convert_exp env rator, List.map (alpha_convert_exp env) rands)
  | CoreSetExp (v, e) -> CoreSetExp (get_alpha_var v, alpha_convert_exp env e)
  | CoreFreeVarExp _ -> failwith "Unreachable"

let alpha_convert_def global_env (CoreDef (var, exp)) =
  match AlphaEnvironment.apply var global_env with
  | Some alpha_name -> CoreDef (alpha_name, alpha_convert_exp global_env exp)
  | None -> failwith @@ "No alphaname generated for definition name: " ^ var

let alpha_convert_program (CoreProg defs) =
  let global_env =
    List.fold_left
      (fun env (CoreDef (var, _)) ->
        AlphaEnvironment.add var (AlphaDefGen.gen_sym ()) env)
      (AlphaEnvironment.new_env ()) defs
  in
  List.map (alpha_convert_def global_env) defs |> CoreProg