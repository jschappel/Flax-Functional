type value = Num of float | Bool of bool

let cmp v1 v2 = 
  match (v1, v2) with 
  | Num(n1), Num(n2) -> n1 = n2
  | Bool(b1), Bool(b2) -> b1 = b2
  | _  -> false