open Token 

type literal =
  | Num of float
  | Bool of bool

type expression =
  | BinaryExpr of tokenType * expression * expression
  | UnaryExpr of tokenType * expression
  | LiteralExpr of literal