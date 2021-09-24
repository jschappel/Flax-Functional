open CoreProgram
open Enviroment
open Value

exception InterpreterError of string


let is_truthy = function
| BoolVal(true) -> false
| _ -> true


let rec value_of (exp: expression) (env: enviroment): value =
  match exp with 
  | BinaryExpr(op, exp1, exp2) -> value_of_binary op exp1 exp2 env
  | LiteralExpr(inner) -> value_of_literal inner env
  | UnaryExpr(op, exp) -> value_of_unary op exp env
  | IfExpr(cond, then_exp, else_exp) -> 
    let cond = is_truthy @@ value_of cond env in
    if cond then value_of then_exp env else value_of else_exp env
  | LetExpr(exp_list, body) ->
    let val_list = List.map (fun (s,exp) -> (s, value_of exp env)) exp_list in
    value_of body @@ List.fold_left ext_env env val_list
  | _ -> raise @@ InterpreterError "Unreachable" 

and value_of_binary op exp1 exp2 env =
  let v1 = value_of exp1 env in
  let v2 = value_of exp2 env in
  match (op, v1, v2) with
  | PLUS, NumVal(n1), NumVal(n2) -> NumVal(n1 +. n2)
  | MINUS, NumVal(n1), NumVal(n2) -> NumVal(n1 -. n2)
  | STAR, NumVal(n1), NumVal(n2) -> NumVal(n1 *. n2)
  | SLASH, NumVal(n1), NumVal(n2) -> NumVal(n1 /. n2)
  | _ -> raise @@ InterpreterError "'+' operator expects two numbers"

and value_of_unary op exp env =
  let v = value_of exp env in
  match op with 
  | NOT when is_truthy v-> BoolVal(false)
  | NOT -> BoolVal(true)
  | _ -> raise @@ InterpreterError "Invalid unary on expression type"

and value_of_literal exp env = 
  match exp with
  | Num(n) -> NumVal(n)
  | Bool(n) -> BoolVal(n)
  | Ident(i) -> 
    (match get_value env i with 
    | Some(v) -> v
    | None -> raise @@ InterpreterError ("Value " ^ i ^ " not found in enviroment"))
  | _ -> raise @@ InterpreterError "Invalid literal Expression Supplied"