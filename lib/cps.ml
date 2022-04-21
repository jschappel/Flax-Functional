module CPS : sig
  open CoreProgram
  open Enviroment.Enviroment

  type program = expression
  type final_answer = value
  type cont = (value -> value)

  (* Returns the value of the program *)
  val value_of_program : program -> value

  (* Returns the value of the given expression useing a cps transformation *)
  val value_of_k : expression -> env -> cont -> value

  val apply_cont : cont -> value -> final_answer

end = struct
  open CoreProgram
  open Enviroment.Enviroment
  open Utils

  type program = expression
  type final_answer = value
  type cont = (value -> value)

  let end_cont v = 
    print_endline "End continuation";
    v
    
  let rec value_of_program prog = value_of_k prog EmptyEnv end_cont

  and value_of_k exp env cont = Todo.unimplimented ()

  and apply_cont end_cont value = Todo.unimplimented ()


end