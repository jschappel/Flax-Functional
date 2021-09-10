open Token

type expression =
  | FuncExpr of string list * expression
  | CallExpr of string * expression list
  | LetExpr of (string * expression) list * expression
  | IfExpr of expression * expression * expression 
  | BinaryExpr of tokenType * expression * expression
  | UnaryExpr of tokenType * expression
  | LiteralExpr of literal
  and literal = Num of float | Bool of bool | Ident of string