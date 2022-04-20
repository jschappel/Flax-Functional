open Token

type expression =
  | FuncExpr of args * expression
  | CallExpr of string * expression list
  | LetExpr of (string * expression) list * expression
  | LetRecExpr of (name * args * expression) list * expression
  | IfExpr of expression * expression * expression
  | BinaryExpr of tokenType * expression * expression
  | UnaryExpr of tokenType * expression
  | LiteralExpr of literal

and literal = Num of float | Bool of bool | Ident of string
and name = string
and args = string list [@@deriving show, eq]
(*
   let
     x = 10
   in
   x + 10


   letrec
     add = fn x, y => x + y;
     y = 10;
   in
    add(x, 10)
*)
