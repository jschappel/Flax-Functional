let rec ormap cond = function
  | [] -> false
  | x :: xs -> if cond x then true else ormap cond xs
