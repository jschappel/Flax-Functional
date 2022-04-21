module CPS : sig
  open CoreProgram
  open Enviroment.Enviroment

  type program = expression

  val value_of_program : program -> value

  val value_of : expression -> env -> value

end = struct
  open CoreProgram
  open Enviroment.Enviroment

  type program = expression

  let value_of_program prog = BoolVal(false)

  let value_of exp env = BoolVal(false)

end