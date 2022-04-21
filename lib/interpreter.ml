open CoreProgram
open Token
open Enviroment.Enviroment

exception InterpreterError of string

let is_truthy = function BoolVal false -> false | _ -> true

(* Determins if a variable occurs free in the given env *)
let rec occurs_free (var : string) (exp : expression) : bool =
  match exp with
  | LiteralExpr (Ident v) -> v = var
  | FuncExpr (params, body) when not (List.mem var params) ->
      occurs_free var body
  | UnaryExpr (_, exp) -> occurs_free var exp
  | BinaryExpr (_, exp1, exp2) -> occurs_free var exp1 || occurs_free var exp2
  | IfExpr (e1, e2, e3) ->
      occurs_free var e1 || occurs_free var e2 || occurs_free var e3
  | CallExpr (_, params) ->
      List.fold_left (fun a e -> a || occurs_free var e) false params
  | LetExpr (exp_list, body) ->
      let free_in_let_exprs =
        List.fold_left (fun a (_, e) -> a || occurs_free var e) false exp_list
      in
      free_in_let_exprs || occurs_free var body
  | _ -> false

(* Creates the optimized enviroment onyl containing the free vars*)
let rec make_free_var_env (env : env) (exp : expression) (acc : pair list) : env
    =
  match env with
  | EmptyEnv -> ext_env acc EmptyEnv
  | ExtEnv (l, ext_env) ->
      let free_vars = acc @ List.filter (fun (s, _) -> occurs_free s exp) l in
      make_free_var_env ext_env exp free_vars
  | _ ->
      print_string "HERERE";
      Utils.Todo.unimplimented ()

let rec value_of (exp : expression) (env : env) : value =
  match exp with
  | BinaryExpr (op, exp1, exp2) -> value_of_binary op exp1 exp2 env
  | LiteralExpr inner -> value_of_literal inner env
  | UnaryExpr (op, exp) -> value_of_unary op exp env
  | IfExpr (cond, then_exp, else_exp) ->
      let cond = is_truthy @@ value_of cond env in
      if cond then value_of then_exp env else value_of else_exp env
  | LetExpr (exp_list, body) ->
      let val_list =
        List.map (fun (s, exp) -> (s, value_of exp env)) exp_list
      in
      value_of body @@ ext_env val_list env
  | FuncExpr (params, body) ->
      let new_env = make_free_var_env env exp [] in
      ProcVal (params, body, new_env)
  | LetRecExpr (exp_list, body) -> value_of body @@ ext_env_rec exp_list env
  | CallExpr (name, params) ->
      let rands = List.map (fun v -> value_of v env) params in
      let rator = value_of (LiteralExpr (Ident name)) env in
      apply_procedure rands name rator

and apply_procedure vals name = function
  | ProcVal (params, body, env) ->
      let param_len = List.length params in
      let vals_len = List.length vals in
      if param_len != vals_len then
        raise
        @@ InterpreterError
             ("Arity mismatch. Expected "
             ^ Int.to_string param_len
             ^ ", Given: "
             ^ Int.to_string vals_len)
      else
        let val_list = List.map2 (fun s v -> (s, v)) params vals in
        value_of body @@ ext_env val_list env
  | _ ->
      raise
      @@ InterpreterError ("Unable to find <func " ^ name ^ "> in enviroment")

and value_of_binary op exp1 exp2 env =
  let v1 = value_of exp1 env in
  let v2 = value_of exp2 env in
  match (op, v1, v2) with
  (* Basic Math operations *)
  | PLUS, NumVal n1, NumVal n2 -> NumVal (n1 +. n2)
  | MINUS, NumVal n1, NumVal n2 -> NumVal (n1 -. n2)
  | STAR, NumVal n1, NumVal n2 -> NumVal (n1 *. n2)
  | SLASH, NumVal n1, NumVal n2 -> NumVal (n1 /. n2)
  | GT, NumVal n1, NumVal n2 -> BoolVal (n1 > n2)
  | LT, NumVal n1, NumVal n2 -> BoolVal (n1 < n2)
  | GT_EQ, NumVal n1, NumVal n2 -> BoolVal (n1 >= n2)
  | LT_EQ, NumVal n1, NumVal n2 -> BoolVal (n1 <= n2)
  | EQ_EQ, v1, v2 -> (
      match (v1, v2) with
      | NumVal n1, NumVal n2 -> BoolVal (n1 = n2)
      | BoolVal b1, BoolVal b2 -> BoolVal (b1 = b2)
      | _ -> BoolVal false)
  | OR, v1, v2 ->
      let t1 = is_truthy v1 in
      let t2 = is_truthy v2 in
      BoolVal (t1 || t2)
  | AND, v1, v2 ->
      let t1 = is_truthy v1 in
      let t2 = is_truthy v2 in
      BoolVal (t1 && t2)
  | _ ->
      raise
      @@ InterpreterError
           (Printf.sprintf "Invalid. Oprerator '%s' expects two numbers"
           @@ show_tokenType op)

and value_of_unary op exp env =
  let v = value_of exp env in
  match op with
  | NOT when is_truthy v -> BoolVal false
  | NOT -> BoolVal true
  | _ -> raise @@ InterpreterError "Invalid unary on expression type"

and value_of_literal exp env =
  match exp with
  | Num n -> NumVal n
  | Bool n -> BoolVal n
  | Ident i -> (
      match get_env_value i env with
      | Some v -> v
      | None ->
          raise @@ InterpreterError ("Value `" ^ i ^ "` not found in enviroment")
      )

let interperet_program exp = value_of exp EmptyEnv
