type value = NumVal of float | BoolVal of bool

let cmp v1 v2 = 
  match (v1, v2) with 
  | NumVal(n1), NumVal(n2) -> n1 = n2
  | BoolVal(b1), BoolVal(b2) -> b1 = b2
  | _  -> false