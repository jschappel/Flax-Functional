open Grammar.CoreGrammar
open Utils.Generator

module AlphaDefGen = SymGen (struct
  let s = "$D$"
end)

module AlphaVarGen = SymGen (struct
  let s = "$V$"
end)

let alpha_convert_exp bound_vars exp = failwith "TODO"

let alpha_convert_def bound_vars (CoreDef (_, exp)) =
  CoreDef (AlphaDefGen.gen_sym (), alpha_convert_exp bound_vars exp)

let alpha_convert_program (CoreProg defs) =
  let bound_vars = List.map (function CoreDef (var, _) -> var) defs in
  List.map (alpha_convert_def bound_vars) defs |> CoreProg
