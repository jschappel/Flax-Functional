(* Utility List functions *)

(** Similar to map in the sense that proc is applied to each element of lst, but the
    result is that of the first application of proc producing a value other than false.

    EX: ormap f [x;y;z] is equivlent to f x || f y || f z *)
val ormap : ('a -> bool) -> 'a list -> bool
