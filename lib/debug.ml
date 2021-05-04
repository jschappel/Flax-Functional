open Lexer
open Core


let token_list_to_string tokens =
  let token_to_string acc token =
    match token with
    | Token(x, PLUS, num) -> (Printf.sprintf "('%s' PLUS %d)" x num) ^ " " ^ acc
    | Token(x, MINUS, num) -> Printf.sprintf "('%s' MINUS %d)" x num
    | Token(x, STAR, num) -> Printf.sprintf "('%s' STAR %d)" x num
    | Token(x, SLASH, num) -> Printf.sprintf "('%s' SLASH %d)" x num
    | Token(x, LEFT_PAREN, num) -> Printf.sprintf "('%s' LEFT_PAREN %d)" x num
    | Token(x, RIGHT_PAREN, num) -> Printf.sprintf "('%s' RIGHT_PAREN %d)" x num
    | Token(x, EQUAL, num) -> Printf.sprintf "('%s' EQUAL %d)" x num
    | Token(x, NUMBER, num) -> Printf.sprintf "('%s'NUMBER %d)" x num
    | Token(x, LET, num) -> Printf.sprintf "('%s' LET %d)" x num
    | Token(x, FUN, num) -> Printf.sprintf "('%s' FUN %d)" x num
    | Token(x, IN, num) -> Printf.sprintf "('%s' IN %d)" x num
    | Token(x, IDENTIFIER, num) -> Printf.sprintf "('%s' IDENTIFIER %d)" x num
    | _ -> "Invlid token given" in
  (List.fold tokens ~init:"" ~f:token_to_string)