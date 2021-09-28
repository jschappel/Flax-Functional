open CoreProgram

type value = 
  NumVal of float 
  | BoolVal of bool
  | FuncVal of string * string list * expression
  [@@deriving show, eq]


let value_to_string = function
| NumVal(n)       -> Float.to_string n
| BoolVal(b)      -> Bool.to_string b
| FuncVal(n,_,_)  -> "<func " ^ n ^ ">"