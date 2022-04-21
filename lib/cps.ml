module CPS : sig
  open CoreProgram
  open Enviroment.Enviroment

  exception CpsError of string

  type program = expression
  type final_answer = value
  type cont = value -> value

  (* Returns the value of the program *)
  val value_of_program : program -> value

  (* Returns the value of the given expression useing a cps transformation *)
  val value_of_k : expression -> env -> cont -> value

  (* Applys the value to the continuation *)
  val apply_cont : cont -> value -> final_answer
end = struct
  open CoreProgram
  open Enviroment.Enviroment
  open Utils

  exception CpsError of string

  type program = expression
  type final_answer = value
  type cont = value -> value

  let end_cont v =
    print_endline "End continuation";
    v

  let rec value_of_program prog = value_of_k prog EmptyEnv end_cont

  and value_of_k exp env cont =
    match exp with
    | LiteralExpr (Num n) -> NumVal n |> apply_cont cont
    | LiteralExpr (Bool b) -> BoolVal b |> apply_cont cont
    | LiteralExpr (Ident i) ->
        let env_val =
          match get_env_value i env with
          | Some v -> v
          | None ->
              raise @@ CpsError ("Value `" ^ i ^ "` not found in enviroment")
        in
        apply_cont cont env_val
    | FuncExpr (args, body) -> ProcVal (args, body, env) |> apply_cont cont
    | LetRecExpr (funcs, body) ->
        let new_env = ext_env_rec funcs env in
        value_of_k body new_env cont
    | LetExpr (vals, body) -> (
        match vals with
        | (s, exp) :: _ -> value_of_k exp env (let_exp_cont s body env cont)
        | _ -> Todo.unimplimented ())
    | IfExpr (exp1, exp2, exp3) ->
        if_test_cont exp2 exp3 env cont |> value_of_k exp1 env
    | BinaryExpr (op, exp1, exp2) ->
        value_of_k exp1 env (diff_cont1 exp2 op env cont)
    | _ -> Todo.unimplimented ()

  and apply_cont cont value = cont value

  and let_exp_cont var body env cont value =
    value_of_k body (ext_env [ (var, value) ] env) cont

  and if_test_cont exp2 exp3 env cont value =
    match value with
    | BoolVal b ->
        if b then value_of_k exp2 env cont else value_of_k exp3 env cont
    | _ -> raise @@ CpsError "Expected bool for if test"

  and diff_cont1 exp2 op env cont val1 =
    value_of_k exp2 env (diff_cont2 val1 op cont)

  and diff_cont2 val1 op cont val2 =
    let new_val = value_of_binary op val1 val2 in
    apply_cont cont new_val

  and value_of_binary op v1 v2 =
    let is_truthy = function BoolVal false -> false | _ -> true in
    match (op, v1, v2) with
    (* Basic Math operations *)
    | PLUS, NumVal n1, NumVal n2 -> NumVal (n1 +. n2)
    | MINUS, NumVal n1, NumVal n2 -> NumVal (n1 -. n2)
    | STAR, NumVal n1, NumVal n2 -> NumVal (n1 *. n2)
    | SLASH, NumVal n1, NumVal n2 -> NumVal (n1 /. n2)
    | GT, NumVal n1, NumVal n2 -> BoolVal (n1 > n2)
    | LT, NumVal n1, NumVal n2 -> BoolVal (n1 < n2)
    | GT_EQ, NumVal n1, NumVal n2 -> BoolVal (n1 >= n2)
    | LT_EQ, NumVal n1, NumVal n2 -> BoolVal (n1 <= n2)
    | EQ_EQ, v1, v2 -> (
        match (v1, v2) with
        | NumVal n1, NumVal n2 -> BoolVal (n1 = n2)
        | BoolVal b1, BoolVal b2 -> BoolVal (b1 = b2)
        | _ -> BoolVal false)
    | OR, v1, v2 ->
        let t1 = is_truthy v1 in
        let t2 = is_truthy v2 in
        BoolVal (t1 || t2)
    | AND, v1, v2 ->
        let t1 = is_truthy v1 in
        let t2 = is_truthy v2 in
        BoolVal (t1 && t2)
    | _ ->
        raise
        @@ CpsError
             (Printf.sprintf "Invalid. Oprerator '%s' expects two numbers"
             @@ Token.show_tokenType op)
end
