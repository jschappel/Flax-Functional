module type Env = sig
  type 'a t

  val empty_env : 'a t
  val ext_env : string * string -> 'a t -> 'a t
  val apply_env : string -> 'a t -> string option
  val env_contains : string -> 'a t -> bool
  val is_prim : string -> bool
end

module Enviroment : Env = struct
  type 'a t =
    | Empty
    | ExtEnv of (string * string) * 'a t

  let empty_env = Empty
  let ext_env p e = ExtEnv (p, e)

  (** TODO: Update this when I know what the second value should be *)
  let apply_env _v _e = None

  let rec env_contains s = function
    | Empty -> false
    | ExtEnv ((t, _), e) -> if String.equal t s then true else env_contains s e
  ;;

  let is_prim s =
    let open Base in
    let exists_in m = Map.find m s |> Option.is_some in
    exists_in Prims.list_prims
    || exists_in Prims.string_prims
    || exists_in Prims.bool_prims
    || exists_in Prims.math_prims
    || exists_in Prims.other_prims
    || exists_in Prims.vect_prims
  ;;
end
