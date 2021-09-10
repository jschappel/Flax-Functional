open Value

type enviroment = 
  EmptyEnv 
  | ExtEnv of pair list * enviroment
  and pair = string * value

val get_value : enviroment -> string -> value option

(* Adds the given pair to the existing outter enviroment *)
val add_value : enviroment -> pair -> enviroment

(* Creates a new enviroment with the given pair added*)
val ext_env : enviroment -> pair -> enviroment