module CPS : sig
  open CoreProgram
  open Enviroment.Enviroment

  type program = expression
  type 'v cont = ('v -> 'v)

  (* Returns the value of the program *)
  val value_of_program : program -> value

  (* Returns the value of the given expression useing a cps transformation *)
  val value_of_k : expression -> env -> 'a cont -> value

end = struct
  open CoreProgram
  open Enviroment.Enviroment

  type program = expression
  type 'v cont = ('v -> 'v)

  let unimplimented _ = raise @@ Failure "unimplimented!"

  let end_cont v = 
    print_endline "End continuation";
    v
    
  let rec value_of_program prog = value_of_k prog EmptyEnv end_cont

  and value_of_k exp env cont = unimplimented ()


end