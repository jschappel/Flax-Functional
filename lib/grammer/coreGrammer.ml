(* This File contains the compiler internal representation of the grammar. This is the
   final form of the grammar and is generated after desugaring the program. *)

(** Grammer Details below:
    - Grammer conts exprs: `CoreNumExp` `CoreBoolExp` `CoreStrExp` `CoreSymExp`
    - `CoreVarExp` includes a string that is the name of the variable
    - `CoreFreeVarExp` is a contains a stirng representing the name of the free
        variable, as well as a unsigned int representing the position of the free
        variable in the closure.
    - `CoreIfExp` includes 3 expressions: 
        1) A boolean test Expression
        2) A expression that is evaluated if ture 
        3) A expression that is evaluated if false
    - `CoreLambdaExp` contains a list of strings that represents the parameters of the
        lambda, an expression for the body of the lambda, and a list of strings that
        represents the free variables of the lambda.
    - `CoreAndExp` is a list of boolean expressions
    - `CoreOrExp` is a list of boolean expressions
    - `CoreNotExp` is an expression to be negated
    - `CoreAppExp` is an expression representing the function that is going to be
        applied, as well as a list of expressions for the input values.
    - `CoreVectorExp` is a list of expressions that will be placed in the vector
    - `CoreListExp` is a list of expressions that will be placed in the list
    - `CoreSetExp` is a variable identifier that represents a variable to be mutated,
        and an expressions that represents the new value the variable should be changed
        to.
    - `CoreBeginExp` is a list of expressions representing the expressions that are
        evaluated in the begin
*)
type core_exp =
  | CoreNumExp of float
  | CoreBoolExp of bool
  | CoreStrExp of string
  | CoreSymExp of string
  | CoreVarExp of string
  | CoreIfExp of core_exp * core_exp * core_exp
  | CoreLambdaExp of string list * core_exp * string list
  | CoreAndExp of core_exp list
  | CoreOrExp of core_exp list
  | CoreNotExp of core_exp
  | CoreAppExp of core_exp * core_exp list
  | CoreVectorExp of core_exp list
  | CoreListExp of core_exp list
  | CoreSetExp of string * core_exp
  | CoreBeginExp of core_exp list
  (* Below are introduced during free var transformation *)
  | CoreFreeVarExp of string * int

(** A variable is an identifier represented as a string *)
and identifier = string [@@deriving show, eq]

(** A CoreDefinition includes a identifier and an expression that represents the body of
    the definition.*)
type core_def = CoreDef of identifier * core_exp [@@deriving show, eq]

(** A CoreProgram is a list of Core Definitions *)
type core_prog = CoreProg of core_def list [@@deriving show, eq]
