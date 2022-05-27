(* This File contains the compiler internal representation of the grammar. This is the
   final form of the grammar and is generated after desugaring the program. *)

type core_exp =
  (* Below are the 4 const expression of the Core Grammar *)
  | CoreNumExp of float
  | CoreBoolExp of bool
  | CoreStrExp of string
  | CoreSymExp of string
      (** A CoreVarExp includes a string that is the name of the variable *)
  | CoreVarExp of string
      (** A CoreFreeVarExp is a contains a stirng representing the name of the free
          variable, as well as a unsigned int representing the position of the free
          variable in the closure. *)
  | CoreFreeVarExp of string * int
      (** A CoreIfExp includes 3 expressions: 1) A boolean test Expression 2) A expression
          that is evaluated if tue 3) A expression that is evaluated if false *)
  | CoreIfExp of core_exp * core_exp * core_exp
      (** A CoreLambdaExp contains a list of strings that represents the parameters of the
          lambda, an expression for the body of the lambda, and a list of strings that
          represents the free variables of the lambda. *)
  | CoreLambdaExp of string list * core_exp * string list
      (** A CoreAndExp is a list of boolean expressions *)
  | CoreAndExp of core_exp list  (** A CoreOrExp is a list of boolean expressions *)
  | CoreOfExp of core_exp list  (** A CoreNotExp is an expression to be negated *)
  | CoreNotExp of core_exp
      (** A CoreAppExp is an expression representing the function that is going to be
          applied, as well as a list of expressions for the input values. *)
  | CoreAppExp of core_exp * core_exp list
      (** A CoreVectorExp is a list of expressions that will be placed in the vector *)
  | CoreVectorExp of core_exp list
      (** A CoreListExp is a list of expressions that will be placed in the list *)
  | CoreListExp of core_exp list
      (** A CoreSetExp is a variable identifier that represents a variable to be mutated,
          and an expressions that represents the new value the variable should be changed
          to. *)
  | CoreSetExp of string * core_exp
  (* A CoreBeginExp is a list of expressions representing the expressions that are
     evaluated in the begin *)
  | CoreBeginExp of core_exp list

(** A core_let_decl is a variable to be declared and a definition for it *)
and core_let_decl = string * core_exp

(** A variable is an identifier represented as a string *)
and identifier = string [@@deriving show]

(** A CoreDefinition includes a identifier and an expression that represents the body of
    the definition.*)
type core_def = CoreDef of identifier * core_exp [@@deriving show]

(** A CoreProgram is a list of Core Definitions *)
type core_prog = CoreProg of core_def list [@@deriving show]
