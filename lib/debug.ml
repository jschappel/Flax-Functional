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
| EQUAL         -> "="
| LET           -> "LET"
| FUN           -> "FUN"
| IN            -> "IN"
| IF            -> "if"
| THEN          -> "then"
| ELSE          -> "else"
| AND           -> "and"
| OR            -> "or"
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


let rec expr_to_string expr = 
  let literal_to_string = function 
    | Num(n) -> Float.to_string n
    | Bool(b) -> Bool.to_string b in 
  match expr with
    | BinaryExpr(op, exp1, exp2) -> "(" ^ (tokenType_to_string op) ^ " " ^ (expr_to_string exp1) ^ " " ^ expr_to_string exp2 ^ ")"
    | LiteralExpr(lit) -> literal_to_string lit