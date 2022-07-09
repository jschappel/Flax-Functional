let rec ormap cond = function
  | [] -> false
  | x :: xs -> if cond x then true else ormap cond xs

let rec andmap cond = function
| [] -> true
| x :: xs -> if cond x then andmap cond xs else false