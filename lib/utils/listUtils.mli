(* Utility List functions *)

(** Similar to map in the sense that proc is applied to each element of lst, but the
    result is that of the first application of proc producing a value other than false.

    EX: ormap f [x;y;z] is equivlent to f x || f y || f z *)
val ormap : ('a -> bool) -> 'a list -> bool

(** Similar to map in the sense that proc is applied to each element of lst, but the
    result is false if any application proc produces false otherwise the result is true.

    EX: andmap f [x;y;z] is equivlent to f x && f y && f z *)
val andmap : ('a -> bool) -> 'a list -> bool
