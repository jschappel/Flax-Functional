open Token
open CoreProgram
open Base

let tokenType_to_string = function
| PLUS          -> "+"
| MINUS         -> "-"
| STAR          -> "*"
| SLASH         -> "/"
| LEFT_PAREN    -> "'('"
| RIGHT_PAREN   -> "')'"
| COMMA         -> ","
| EQ            -> "="
| EQ_EQ         -> "=="
| GT            -> ">"
| GT_EQ         -> ">="
| LT            -> "<"
| LT_EQ         -> "<="
| NOT_EQ        -> "!="
| LET           -> "LET"
| FUN           -> "FUN"
| IN            -> "IN"
| IF            -> "if"
| THEN          -> "then"
| ELSE          -> "else"
| AND           -> "and"
| OR            -> "or"
| NOT            -> "not"
| BOOL(b)       -> if b then "true" else "false"
| NUMBER(n)     -> Float.to_string n
| STRING(s)     -> "\"" ^ s ^ "\""
| IDENTIFIER(i) -> i
| _             -> "Invalid tokentype"


let token_to_string token =
  let Token(tt, _) = token in 
  tokenType_to_string tt

let token_list_to_string tokens =
  List.fold tokens ~init:"" ~f:(fun _ t -> token_to_string t)

let str_list_to_string l =
  let rec helper l acc =
  match l with 
  | [] -> (String.of_char_list @@ List.rev @@ List.drop (List.rev @@ String.to_list acc) 2) ^ "]"
  | (x::xs) -> helper xs (acc ^ x ^ ", ") in
  helper l "["


let rec expr_to_string expr = 
  let trim_end s =
    String.of_char_list @@ List.rev @@ List.drop_while (List.rev @@ String.to_list s) ~f:(Char.equal ' ') in
  let literal_to_string = function 
    | Num(n) -> Float.to_string n
    | Bool(b) -> Bool.to_string b
    | Ident(i) -> i in 
  match expr with
    | FuncExpr(args, body) -> "(lambda " ^ (str_list_to_string args) ^ " " ^ (expr_to_string body) ^ ")"
    | CallExpr(name, args) ->  let exprs = List.fold_left args ~init: "[" ~f:(fun a e -> a ^ (expr_to_string e) ^ " ") in
      "(" ^ name ^ " " ^ (trim_end exprs) ^ "])"
    | LetExpr(l, body) -> let exprs = List.fold_left l ~init: "(let [" ~f:(fun a (s,e) -> a ^ "(" ^ s ^ " = " ^(expr_to_string e) ^ ") ") in
      (trim_end exprs) ^ "] in " ^ (expr_to_string body) ^ ")"
    | IfExpr(exp1, exp2, exp3) -> "(if " ^ (expr_to_string exp1) ^ " " ^ (expr_to_string exp2) ^ " " ^ (expr_to_string exp3) ^ ")"
    | BinaryExpr(op, exp1, exp2) -> "(" ^ (tokenType_to_string op) ^ " " ^ (expr_to_string exp1) ^ " " ^ expr_to_string exp2 ^ ")"
    | UnaryExpr(op, exp) -> "(" ^ (tokenType_to_string op) ^ " " ^ (expr_to_string exp) ^ ")"
    | LiteralExpr(lit) -> literal_to_string lit
    

let val_to_string = function
| Value.Num(n) -> Float.to_string n
| Value.Bool(b) -> Bool.to_string b