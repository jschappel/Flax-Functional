(* This File contains the compiler internal representation of the grammar. This is the
   final form of the grammar and is generated after desugaring the program.

   Note: Phase2 Grammer is introduced during closeure conversion *)

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
  | CorePhase2RefExp of {
      env_ref : string;
      id : identifier;
    }
  | CorePhase2ClosureExp of {
      body : core_exp;
      ref_id : string;
      env : closure_env_exp list;
    }

and closure_env_exp = identifier * closure_env

and closure_env =
  | EnvRef of string * string
  | Var of identifier

(** A variable is an identifier represented as a string *)
and identifier = string [@@deriving show, eq]

(** A CoreDefinition includes a identifier and an expression that represents the body of
    the definition.*)
type core_def = CoreDef of identifier * core_exp [@@deriving show, eq]

(** A CoreProgram is a list of Core Definitions *)
type core_prog = CoreProg of core_def list [@@deriving show, eq]
