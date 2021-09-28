open CoreProgram

type enviroment = 
  EmptyEnv 
  | ExtEnv of pair list * enviroment
and pair = string * value
and value = 
  NumVal of float 
  | BoolVal of bool
  | FuncVal of string list * expression * enviroment
  [@@deriving show, eq]

val get_value : enviroment -> string -> value option

(* Adds the given pair to the existing outter enviroment *)
val add_value : enviroment -> pair -> enviroment

(* Creates a new enviroment with the given pair added*)
val ext_env : enviroment -> pair -> enviroment

val value_to_string : value -> string