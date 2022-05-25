let math_prims =
  Base.Map.of_alist_exn
    (module Base.String)
    [ "sub1", Some 1
    ; "add1", Some 1
    ; "sqr", Some 1
    ; "abs", Some 1
    ; "round", Some 1
    ; "floor", Some 1
    ; "ceiling", Some 1
    ; "truncate", Some 1
    ; "numerator", Some 1
    ; "denominator", Some 1
    ; "exp", Some 1
    ; "sqrt", Some 1
    ; "sin", Some 1
    ; "cos", Some 1
    ; "tan", Some 1
    ; "asin", Some 1
    ; "acos", Some 1
    ; "atan", Some 1
    ; "magnitude", Some 1
    ; "angle", Some 1
    ; "random", Some 1
    ; "exact->inexact", Some 1
    ; "inexact->exact", Some 1
    ; "number->string", Some 1
    ; "+", None
    ; "=", None
    ; "-", None
    ; "*", None
    ; "/", None
    ; "<", None
    ; ">", None
    ; "^", None
    ; "&", None
    ; ">=", None
    ; "<=", None
    ; "quotient", None
    ; "remainder", None
    ; "modulo", None
    ; "max", None
    ; "min", None
    ; "gcd", None
    ; "lcm", None
    ; "log", None
    ]
;;

let string_prims =
  Base.Map.of_alist_exn
    (module Base.String)
    [ "string-upcase", Some 1
    ; "string-length", Some 1
    ; "string->number", Some 1
    ; "string-foldcase", Some 1
    ; "string-downcase", Some 1
    ; "string-titlecase", Some 1
    ; "string-normalize-spaces", Some 1
    ; "substring", Some 3
    ; "string-replace", Some 3
    ; "string-append", None
    ; "build-string", None
    ; "string-join", None
    ; "string=?", None
    ; "string<?", None
    ; "string>?", None
    ; "string<=?", None
    ; "string>=?", None
    ; "string-ci=?", None
    ; "string-ci>?", None
    ; "string-ci<?", None
    ; "string-ci>=?", None
    ; "string-ci<=?", None
    ]
;;

let bool_prims =
  Base.Map.of_alist_exn
    (module Base.String)
    [ "string?", Some 1
    ; "number?", Some 1
    ; "boolean?", Some 1
    ; "integer?", Some 1
    ; "exact-integer?", Some 1
    ; "symbol?", Some 1
    ; "exact-nonnegative-integer?", Some 1
    ; "exact-positive-integer?", Some 1
    ; "positive?", Some 1
    ; "zero?", Some 1
    ; "negative?", Some 1
    ; "even?", Some 1
    ; "odd?", Some 1
    ; "exact?", Some 1
    ; "inexact?", Some 1
    ; "equal?", Some 2
    ; "eqv?", Some 2
    ; "eq?", Some 2
    ; "boolean=?", Some 2
    ]
;;

let list_prims =
  Base.Map.of_alist_exn
    (module Base.String)
    [ "car", Some 1
    ; "cdr", Some 1
    ; "first", Some 1
    ; "rest", Some 1
    ; "null", Some 1
    ; "emptylist", Some 1
    ; "empty", Some 1
    ; "null?", Some 1
    ; "empty?", Some 1
    ; "length", Some 1
    ; "reverse", Some 1
    ; "caar", Some 1
    ; "cadr", Some 1
    ; "cdar", Some 1
    ; "cddr", Some 1
    ; "caaar", Some 1
    ; "caadr", Some 1
    ; "cadar", Some 1
    ; "caddr", Some 1
    ; "cdaar", Some 1
    ; "cdadr", Some 1
    ; "cddar", Some 1
    ; "cdddr", Some 1
    ; "caaaar", Some 1
    ; "caaadr", Some 1
    ; "caadar", Some 1
    ; "caaddr", Some 1
    ; "cadaar", Some 1
    ; "cadadr", Some 1
    ; "caddar", Some 1
    ; "cadddr", Some 1
    ; "cdaaar", Some 1
    ; "cdaadr", Some 1
    ; "cdadar", Some 1
    ; "cdaddr", Some 1
    ; "cddaar", Some 1
    ; "cddadr", Some 1
    ; "cdddar", Some 1
    ; "cddddr", Some 1
    ; "cons", Some 2
    ; "build-list", Some 2
    ; "list-ref", Some 2
    ; "append", Some 2
    ; "map", Some 2
    ; "andmap", Some 2
    ; "ormap", Some 2
    ; "for-each", Some 2
    ; "filter", Some 2
    ; "remove", Some 2
    ; "sort", Some 2
    ; "member", Some 2
    ; "make-list", Some 2
    ; "take", Some 2
    ; "drop", Some 2
    ; "foldl", Some 3
    ; "foldr", Some 3
    ]
;;

let vect_prims =
  Base.Map.of_alist_exn
    (module Base.String)
    [ "array-length", Some 1
    ; "allocate-array", Some 1
    ; "build-array", Some 2
    ; "array-set!", Some 2
    ; "array-ref", Some 2
    ; "vector-set!", Some 2
    ; "vector-ref", Some 2
    ]
;;

(* TODO: Check to make sure the arity on these are correct *)
let other_prims =
  Base.Map.of_alist_exn
    (module Base.String)
    [ "bitwise-or", Some 1
    ; "bitwise-and", Some 1
    ; "bitwise-xor", Some 1
    ; "bitwise-not", Some 1
    ; "bitwise-bit-set?", Some 1
    ; "arithmetic-shift", Some 1
    ; "integer-length", Some 1
    ; "end-k", Some 1
    ; "error", Some 1
    ; "void", Some 0
    ; "void?", Some 1
    ; "pop", Some 0
    ; "push", Some 0
    ]
;;
