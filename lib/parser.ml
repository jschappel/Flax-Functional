open Token
open CoreProgram

exception ParseError of string

let rec parse_expression exp = 
  let (p, _) = parse_and_or_expr exp in p 

and parse_and_or_expr tokens =
  let (exp1, xs) = parse_add_sub_expr tokens in
  let rec loop (l: token list) (acc: expression) = 
    match l with
    | (Token(op, _)::xs) when op = AND || op = OR -> 
      let (exp2, xs) = parse_add_sub_expr xs in                            
      loop xs (BinaryExpr(op, acc, exp2))
    | _ -> (acc, l) in
  loop xs exp1

and parse_add_sub_expr tokens =
  let (exp1, xs) = parse_mult_div_expr tokens in
  let rec loop (l: token list) (acc: expression) = 
    match l with
    | (Token(op, _)::xs) when op = PLUS || op = MINUS -> 
      let (exp2, xs) = parse_literal_exp xs in                            
      loop xs (BinaryExpr(op, acc, exp2))
    | _ -> (acc, l) in
  loop xs exp1

and parse_mult_div_expr tokens =
  let (exp1, xs) = parse_literal_exp tokens in
  let rec loop (l: token list) (acc: expression) = 
    match l with
    | (Token(op, _)::xs) when op = SLASH || op = STAR -> 
      let (exp2, xs) = parse_literal_exp xs in                            
      loop xs (BinaryExpr(op, acc, exp2))
    | _ -> (acc, l) in
  loop xs exp1

and parse_literal_exp = function
| Token(NUMBER(num), _)::xs -> (LiteralExpr(Num(num)), xs)
| Token(BOOL(b), _)::xs -> (LiteralExpr(Bool(b)), xs)
| Token(_, line)::_ -> raise @@ ParseError("Invalid Token supplied at line " ^ Int.to_string line)
| [] -> raise @@ ParseError("Unreachable")