open Lexer
open Token
open CoreProgram
open Base

exception ParseError of string

let parse_expression _tokens = NumExpr(12.0)


let parse_number_exp tokens = function
| Token(num) -> NumExpr(num)
| _ -> raise @@ ParseError "Invalid token supplied"