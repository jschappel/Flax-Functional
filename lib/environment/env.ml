module type ENV = sig
  type key
  type 'a t

  val new_env : unit -> 'a t
  val ext : key -> 'a -> 'a t -> 'a t
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

  let ext _ _ _ = failwith "Prim Env can not be extended"

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
  let ext k v env = ExtEnv ([ k, v ], env)

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
