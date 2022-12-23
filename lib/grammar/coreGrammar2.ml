(* Second phase of the core grammar where lambda expressions are converted to closures for
   C code generation. *)

type core_exp_phase2 =
  | Core2NumExp of float
  | Core2BoolExp of bool
  | Core2StrExp of string
  | Core2SymExp of string
  | Core2VarExp of string
  | Core2IfExp of core_exp_phase2 * core_exp_phase2 * core_exp_phase2
  | Core2AndExp of core_exp_phase2 list
  | Core2OrExp of core_exp_phase2 list
  | Core2NotExp of core_exp_phase2
  | Core2AppExp of core_exp_phase2 * core_exp_phase2 list
  | Core2VectorExp of core_exp_phase2 list
  | Core2ListExp of core_exp_phase2 list
  | Core2SetExp of string * core_exp_phase2
  | Core2BeginExp of core_exp_phase2 list
  | Core2ClosureExp of {
      body : core_exp_phase2;
      env : core_exp_phase2;
    }
  | Core2MakeEnvExp of {
      name : string;
      values : string * core_exp_phase2;
    }
  | Core2EnvGetExp of {
      name : string;
      id : string;
    }
[@@deriving show, eq]

type core_def_phase2 = Core2Def of string * core_exp_phase2 [@@deriving show, eq]

type core_prog_phase2 = Core2Prog of core_def_phase2 list [@@deriving show, eq]
