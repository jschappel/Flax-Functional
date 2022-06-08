open CoreGrammer
open Utils
open Flax_environment

module SymGen = struct
  let counter = ref 0

  let gen_sym () =
    counter := !counter + 1;
    Printf.sprintf "v%d" !counter

  let reset () = counter := 0
end

let gen_cont_name = Printf.sprintf "%s/k"

let rec ormap cond = function
  | [] -> false
  | x :: xs -> if cond x then true else ormap cond xs

let rec get_fist_non_tailcall e =
  match e with
  | CoreNumExp _ | CoreSymExp _ | CoreStrExp _ | CoreVarExp _ | CoreBoolExp _ -> None
  | CoreIfExp (e1, _, _) -> get_fist_non_tailcall e1
  | CoreLambdaExp (_, _, _) -> None
  | CoreOrExp exps -> get_fist_non_tailcall_in_list exps
  | CoreAndExp exps -> get_fist_non_tailcall_in_list exps
  | CoreNotExp e1 -> get_fist_non_tailcall e1
  | CoreAppExp (CoreVarExp r, rands) when Env.Enviroment.is_prim r ->
      get_fist_non_tailcall_in_list rands
  | CoreAppExp (_, _) -> Some e
  | CoreVectorExp exps -> get_fist_non_tailcall_in_list exps
  | CoreListExp exps -> get_fist_non_tailcall_in_list exps
  | CoreSetExp (_, e) -> get_fist_non_tailcall e
  | CoreBeginExp e -> get_fist_non_tailcall_in_list e
  | CoreFreeVarExp _ -> failwith "Unreachable"

and get_fist_non_tailcall_in_list = function
  | [] -> None
  | x :: xs -> (
      match get_fist_non_tailcall x with
      | Some _ -> Some x
      | None -> get_fist_non_tailcall_in_list xs)

let rec occurs_free var = function
  | CoreNumExp _ | CoreBoolExp _ | CoreSymExp _ | CoreStrExp _ -> false
  | CoreVarExp v -> String.equal v var
  | CoreIfExp (e1, e2, e3) ->
      occurs_free var e1 || occurs_free var e2 || occurs_free var e3
  | CoreLambdaExp (p, b, _) -> (not @@ List.mem var p) && occurs_free var b
  | CoreOrExp exps -> ormap (occurs_free var) exps
  | CoreAndExp exps -> ormap (occurs_free var) exps
  | CoreNotExp e -> occurs_free var e
  | CoreAppExp (r, rands) -> [ r ] |> List.append rands |> ormap (occurs_free var)
  | CoreVectorExp exps -> ormap (occurs_free var) exps
  | CoreListExp exps -> ormap (occurs_free var) exps
  | CoreSetExp (_, e) -> occurs_free var e
  | CoreBeginExp exps -> ormap (occurs_free var) exps
  | CoreFreeVarExp _ -> failwith "Unreachable"

(* replaces the old expression with the new expression inside the target *)
let sub_exp old_e new_e target_e =
  let sub_in_list e = if equal_core_exp old_e e then new_e else e in
  match target_e with
  | CoreNumExp _ | CoreBoolExp _ | CoreStrExp _ | CoreVarExp _ | CoreLambdaExp (_, _, _)
    ->
      target_e
  | CoreSymExp s1 -> (
      match old_e with CoreSymExp s2 when String.equal s1 s2 -> new_e | _ -> target_e)
  | CoreIfExp (_, _, _) -> failwith "Can not sub in a CoreIfExp"
  | CoreNotExp _ -> CoreNotExp new_e
  | CoreAppExp (r, rands) ->
      if equal_core_exp r old_e then CoreAppExp (new_e, rands)
      else CoreAppExp (r, List.map sub_in_list rands)
  | CoreOrExp exps -> List.map sub_in_list exps |> CoreOrExp
  | CoreAndExp exps -> List.map sub_in_list exps |> CoreAndExp
  | CoreVectorExp exps -> List.map sub_in_list exps |> CoreVectorExp
  | CoreListExp exps -> List.map sub_in_list exps |> CoreVectorExp
  | CoreBeginExp exps -> List.map sub_in_list exps |> CoreBeginExp
  | CoreSetExp (t, _) -> CoreSetExp (t, new_e)
  | CoreFreeVarExp _ -> failwith "Unreachable"

let rec cps_prog (CoreProg defs) =
  let decide_cps d =
    match d with
    | CoreDef (n, (CoreLambdaExp (_, _, _) as l)) -> cps_def_lambda n l
    | _ -> Todo.unimplimented ()
  in
  List.map decide_cps defs |> List.flatten |> CoreProg

and cps_def_lambda name (CoreLambdaExp (params, body, _)) =
  let mk_core_var p = CoreVarExp p in
  let new_name = gen_cont_name name in
  let exp_1 =
    CoreLambdaExp
      ( params,
        CoreAppExp
          ( CoreVarExp new_name,
            [ Constants.end_cont ] |> List.append (List.map mk_core_var params) ),
        [] )
  in
  let cps_exp_1 =
    CoreLambdaExp
      (List.append params [ "$k$" ], cps_exp body Constants.continuation_var, [])
  in
  [ CoreDef (name, exp_1); CoreDef (new_name, cps_exp_1) ]

(* Pull out the first expression that has a nontail call if it exists. CPS this expression
   with a new cont where said expression is substituted with the parameter of the new
   cont. The first expression, e, with a nontail call is e if e is a function call or e
   contains a nontail call*)
and cps_exp exp k =
  let helper k =
    match k with
    | CoreVarExp _ -> CoreAppExp (k, [ exp ])
    | CoreLambdaExp (p :: _, b, _) when occurs_free p b -> CoreAppExp (k, [ exp ])
    | CoreLambdaExp (_, b, _) -> b
    | _ -> failwith "Unreachable"
  in
  match exp with
  | CoreNumExp _ | CoreBoolExp _ | CoreSymExp _ | CoreStrExp _ | CoreVarExp _ -> helper k
  | CoreAndExp _ | CoreNotExp _ | CoreOrExp _ | CoreVectorExp _ | CoreListExp _ ->
      mk_cps_exp exp k
  | CoreIfExp (e1, e2, e3) -> (
      match get_fist_non_tailcall e1 with
      | None -> CoreIfExp (e1, cps_exp e2 k, cps_exp e3 k)
      | Some e ->
          let k_param = SymGen.gen_sym () in
          let new_e1 = sub_exp e (CoreVarExp k_param) e1 in
          CoreLambdaExp ([ k_param ], cps_exp (CoreIfExp (new_e1, e2, e3)) k, [])
          |> cps_exp e)
  | CoreLambdaExp (p, b, _) ->
      let new_params = List.append p [ "$k$" ] in
      CoreAppExp
        (k, [ CoreLambdaExp (new_params, cps_exp b Constants.continuation_var, []) ])
  | CoreAppExp (rator, rands) -> (
      match get_fist_non_tailcall rator with
      | Some rator ->
          (* When not in tail call cps the operator and create a new closure whose body is
             the cps'ed expression of substituting the operator with the variable that
             represents its value *)
          let k_param = SymGen.gen_sym () in
          let body = cps_exp (sub_exp rator (CoreVarExp k_param) exp) k in
          CoreLambdaExp ([ k_param ], body, []) |> cps_exp rator
      | None -> (
          (* If op is lambda-exp then add the cont param and cps the body *)
          let new_rator =
            match rator with
            | CoreLambdaExp (p, b, _) ->
                CoreLambdaExp (p @ [ "$k$" ], cps_exp b Constants.continuation_var, [])
            | _ -> rator
          in
          match (get_fist_non_tailcall_in_list rands, rator) with
          | None, CoreVarExp r when Env.Enviroment.is_prim r ->
              (* Update lambdas if any...*)
              CoreAppExp (k, [ CoreAppExp (new_rator, update_lambdas rands) ])
          | None, _ ->
              (* Update lambdas if any...*)
              CoreAppExp (new_rator, update_lambdas rands @ [ k ])
          | Some fnt, _ ->
              let k_param = SymGen.gen_sym () in
              let body = cps_exp (sub_exp fnt (CoreVarExp k_param) exp) k in
              cps_exp exp (CoreLambdaExp ([ k_param ], body, []))))
  | CoreBeginExp _exps -> failwith "TODO: Unimpilmented"
  | CoreSetExp (_i, _e) -> failwith "TODO: Unimpilmented"

and mk_cps_exp target_exp k =
  match get_fist_non_tailcall target_exp with
  | None -> CoreAppExp (k, [ target_exp ])
  | Some victim ->
      let k_param = SymGen.gen_sym () in
      cps_exp victim
        (CoreLambdaExp
           ([ k_param ], cps_exp (sub_exp victim (CoreVarExp k_param) target_exp) k, []))

and update_lambdas _exp_lst = Todo.unimplimented ()
