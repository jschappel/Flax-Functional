module type Env = sig
  open Base
  open CoreProgram

  type identifier = string

  type env =
    | EmptyEnv
    | ExtEnv of pair list * env
    | ExtEnvRec of identifier * identifier * expression * env

  and pair = identifier * value

  and value =
    | NumVal of float
    | BoolVal of bool
    | ProcVal of identifier list * expression * env
  [@@deriving show, eq]

  (* returns a empty env *)
  val empty_env : env

  (* Extends the enviroment with the given value*)
  val ext_env : pair list -> env -> env

  (* Trys the find a value that is in a env *)
  val get_env_value : string -> env -> value option
end

module Enviroment : Env = struct
  open Base
  open CoreProgram

  type env =
    | EmptyEnv
    | ExtEnv of pair list * env
    | ExtEnvRec of identifier * identifier * expression * env

  and pair = identifier * value

  and value =
    | NumVal of float
    | BoolVal of bool
    | ProcVal of identifier list * expression * env

  and identifier = string [@@deriving show, eq]

  let empty_env = EmptyEnv

  let ext_env p = function
    | EmptyEnv -> ExtEnv (p, EmptyEnv)
    | e -> ExtEnv (p, e)

  let rec get_env_value target env =
    match env with
    | EmptyEnv -> None
    | ExtEnv (pairs, ext) -> (
        let comparator (k, _) = equal_string k target in
        match List.find pairs ~f:comparator with
        | Some (_, v) -> Some v
        | None -> get_env_value target ext)
    | ExtEnvRec (fname, bound_var, fbody, saved_env) ->
        if equal_string target fname then
          Some (ProcVal ([ bound_var ], fbody, env))
        else get_env_value target saved_env
end
