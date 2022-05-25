module type Env = sig
  (** The type of a environment variable *)
  type 'a t

  (** Returns an empty environment *)
  val empty_env : 'a t

  (** [ext_env p e] extends the environment by adding the given pair *)
  val ext_env : string * string -> 'a t -> 'a t

  (** [apply_env s e] Applies the given input var into environment. *)
  val apply_env : string -> 'a t -> string option

  (** [env_contains s e] Checks to see if a variable exists in the environment *)
  val env_contains : string -> 'a t -> bool

  (** [is_prim s] Returns true if the given var is a procedure in the primitive environment *)
  val is_prim : string -> bool
end

module Enviroment : Env
