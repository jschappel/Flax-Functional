module type ENV = sig
  (** The type of a environment variable *)
  type key

  type 'a t

  (** Returns an new environment *)
  val new_env : unit -> 'a t

  (** [ext p e] extends the environment by adding the given pair *)
  val ext : key -> 'a -> 'a t -> 'a t

  (** [add p e] adds the given pair to the current lexical scope in the environment *)
  val add : key -> 'a -> 'a t -> 'a t

  (** [apply s e] Applies the given input var into environment. *)
  val apply : key -> 'a t -> 'a option

  (** [contains s e] Checks to see if a variable exists in the environment *)
  val contains : key -> 'a t -> bool
end

(** Enviroment containing the primitive functions *)
module PrimEnvironment : ENV with type key = string

(** Environnment containing the alpha coonversion variables *)
module AlphaEnvironment : ENV with type key = string
