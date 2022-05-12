type var = string

type exp = 
  | NumExp of float
  | BoolExp of bool
  | SymExp of string
  | StrExp of string
  | VarExp of var
  | IfExp of exp * exp * exp
  | CondExp of (exp * exp) list
  | LambdaExp of var list * exp
  | LetExp of (var * exp) list * exp
  | AndExp of exp list
  | OrExp of exp list
  | NotExp of exp
  | AppExp of exp * exp list 
  | VectorExp of exp list
  | ListExp of exp list
  | SetExp of var * exp
  | BeginExp of exp list

type def = 
  | Def of var * exp
  | DefFunc of var * var list * exp


type program = Program of def list
