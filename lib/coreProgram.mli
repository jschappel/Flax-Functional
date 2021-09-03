open Token 

type literal =
  | Num of float
  | Bool of bool

type expression =
  | BinaryExpr of tokenType * expression * expression
  | LiteralExpr of literal