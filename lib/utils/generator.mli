module type Sym = sig
  (** The base value that all unique symbols will generate off of *)
  val s : string
end

module SymGen : functor (S : Sym) -> sig
  (** generates a new unique symbol by appending a number to the base value *)
  val gen_sym : unit -> string

  (** reset the symbol generator back to 0 *)
  val reset : unit -> unit
end

module type SymFmt = sig 
  val fmt: int -> string
end

module SymGenFmt : functor (S : SymFmt) -> sig
  (** generates a new unique symbol by appending a number to the base value *)
  val gen_sym : unit -> string

  (** reset the symbol generator back to 0 *)
  val reset : unit -> unit
end