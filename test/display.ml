open Lib

(* module type DisplayToken = sig
  type t 
  val display : t -> string
end *)

module DisplayToken = Lib.Display(struct
  type t = token
  let display t = "Hello"
end)