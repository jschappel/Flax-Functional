module type Env = sig
  open Base
  open CoreProgram

  type identifier = string
  type args = string list

  type env =
    | EmptyEnv
    | ExtEnv of pair list * env
    | ExtEnvRec of (identifier * args * expression) list * env

  and pair = identifier * value

  and value =
    | NumVal of float
    | BoolVal of bool
    | ProcVal of args * expression * env
  [@@deriving show, eq]

  (* returns a empty env *)
  val empty_env : env

  (* Extends the enviroment with the given value*)
  val ext_env : pair list -> env -> env
  val ext_env_rec : (identifier * args * expression) list -> env -> env

  (* Trys the find a value that is in a env *)
  val get_env_value : string -> env -> value option
end

module Enviroment : Env = struct
  open Base
  open CoreProgram

  type args = string list

  type env =
    | EmptyEnv
    | ExtEnv of pair list * env
    | ExtEnvRec of (identifier * args * expression) list * env

  and pair = identifier * value

  and value =
    | NumVal of float
    | BoolVal of bool
    | ProcVal of args * expression * env

  and identifier = string [@@deriving show, eq]

  let empty_env = EmptyEnv
  let ext_env p e = ExtEnv (p, e)
  let ext_env_rec p e = ExtEnvRec (p, e)

  let rec get_env_value target env =
    match env with
    | EmptyEnv -> None
    | ExtEnv (pairs, ext) -> (
        let comparator (k, _) = equal_string k target in
        match List.find pairs ~f:comparator with
        | Some (_, v) -> Some v
        | None -> get_env_value target ext)
    | ExtEnvRec (funcs, saved_env) -> (
        let res =
          List.find funcs ~f:(fun (fname, _, _) -> equal_string fname target)
        in
        match res with
        | Some (_, bound_vars, fbody) -> Some (ProcVal (bound_vars, fbody, env))
        | None -> get_env_value target saved_env)
end
