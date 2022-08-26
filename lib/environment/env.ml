module type ENV = sig
  type key
  type 'a t

  val new_env : unit -> 'a t
  val new_scope : 'a t -> 'a t
  val ext : key -> 'a -> 'a t -> 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val apply : key -> 'a t -> 'a option
  val contains : key -> 'a t -> bool
end

module PrimEnvironment : ENV with type key = string = struct
  type key = string
  type 'a t = (key, int option, Base.String.comparator_witness) Base.Map.t list

  let new_env () =
    [ Prims.list_prims
    ; Prims.string_prims
    ; Prims.bool_prims
    ; Prims.math_prims
    ; Prims.other_prims
    ; Prims.vect_prims
    ]
  ;;

  let new_scope _ = failwith "Can not add a new scope to the prim env"
  let ext _ _ _ = failwith "Prim Env can not be extended"
  let add _ _ _ = failwith "New values/functions can not be added the the Prim Env"

  (** TODO: Update this when I know what the second value should be *)
  let apply _v _e = None

  let contains s env =
    let open Base in
    let open Utils in
    let exists_in m = Map.find m s |> Option.is_some in
    ListUtils.ormap exists_in env
  ;;
end

module AlphaEnvironment : ENV with type key = string = struct
  type key = string

  type 'a t =
    | Empty
    | ExtEnv of (key * 'a) list * 'a t

  let get_value target (v, _) = String.equal target v
  let new_env () = Empty
  let new_scope env = ExtEnv ([], env)
  let ext k v env = ExtEnv ([ k, v ], env)

  let add k v = function
    | ExtEnv (env, ext_env) -> ExtEnv ((k, v) :: env, ext_env)
    | Empty -> ExtEnv ([ k, v ], Empty)
  ;;

  let rec apply target = function
    | Empty -> None
    | ExtEnv (convs, env) ->
      (match List.find_opt (get_value target) convs with
       | Some (_, a) -> Some a
       | None -> apply target env)
  ;;

  let rec contains target = function
    | ExtEnv (convs, ext_env) ->
      if List.exists (get_value target) convs then true else contains target ext_env
    | Empty -> false
  ;;
end
