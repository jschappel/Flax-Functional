module Utils = Utils
module Parser = Flax_parser.ParserInterface
module Desugarar = Desugarar
module Cps = Cps

module Build = struct
  let run_prog prog =
    prog |> Parser.parse_program |> Desugarar.desugar_program |> Cps.cps_program

  let show_core_prog core_prog =  
    CoreGrammer.show_core_prog core_prog
end

module Test = struct
  module Ast = Flax_parser.Ast
end
