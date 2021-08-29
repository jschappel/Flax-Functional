type tokenType = 
    LEFT_PAREN 
    | RIGHT_PAREN 
    | EQUAL
    | PLUS 
    | MINUS 
    | STAR 
    | SLASH 
    | DOT
    | NUMBER
    | LET 
    | IN 
    | FUN
    | IDENTIFIER

type token = Token of string * tokenType * int

let display = function
  | Token(x, PLUS, num) -> (Printf.sprintf "('%s' PLUS %d)" x num)
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
  | _ -> "Invlid token given"

let equal (Token(l, t, n)) (Token(l2, t2, n2)) = l == l2 && t == t2 && n == n2
