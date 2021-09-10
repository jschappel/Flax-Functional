open Value
open Base

type enviroment = 
  EmptyEnv 
  | ExtEnv of pair list * enviroment
  and pair = string * value

let rec get_value env value =
  match env with 
  | EmptyEnv -> None
  | ExtEnv(pairs, ext_env) -> 
    let comparator (k, _) = equal_string k value in
    (match List.find pairs ~f:comparator with
    | Some(_,v) -> Some(v)
    | None -> get_value ext_env value)

let add_value env pair = 
  match env with
  | EmptyEnv -> ExtEnv([pair], EmptyEnv)
  | ExtEnv(l, env) -> ExtEnv(pair::l, env)

let ext_env env pair =
  match env with
  | EmptyEnv -> ExtEnv([pair], EmptyEnv)
  | env -> ExtEnv([pair], env)