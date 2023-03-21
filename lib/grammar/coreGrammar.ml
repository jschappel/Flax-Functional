(* This File contains the compiler internal representation of the grammar. This is the
   final form of the grammar and is generated after desugaring the program. *)

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
  (* Below is introduced during closure conversion *)
  (* TODO: Removed below?*)
  | CorePhase2EnvRefExp of {
      env : core_exp;
      var : string;
    }
  | CorePhase2MakeEnvExp of (identifier * core_exp) list
  | CorePhase2MakeClosureExp of {
      lambda : core_exp;
      env : core_exp;
    }
  | CorePhase2LambdaExp of {
      params : string list;
      body : core_exp;
    }

(** A variable is an identifier represented as a string *)
and identifier = string [@@deriving show, eq]

(** A CoreDefinition includes a identifier and an expression that represents the body of
    the definition.*)
type core_def = CoreDef of identifier * core_exp [@@deriving show, eq]

(** A CoreProgram is a list of Core Definitions *)
type core_prog = CoreProg of core_def list [@@deriving show, eq]
