module Utils = Utils
module Parser = Flax_parser.ParserInterface
module Desugarar = Desugarar
module Cps = Cps
module Grammar = Flax_grammar

module Build = struct
  let run_prog prog =
    prog
    |> Flax_parser.ParserInterface.parse_program
    |> Desugarar.desugar_program
    |> Cps.cps_program

  let show_core_prog core_prog = Grammar.CoreGrammar.show_core_prog core_prog
end
