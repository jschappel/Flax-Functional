(** This File contains the source grammar of the compiler. This is the grammar
    that is provided by the user and contains syntatic sugar constructs are will
    later be broken down by the compiler. *)

type var = string [@@deriving show]

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
[@@deriving show]

type def = Def of var * exp | DefFunc of var * var list * exp
[@@deriving show]

type program = Program of def list [@@deriving show]
