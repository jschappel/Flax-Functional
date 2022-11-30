open Stdlib
open Flax_core.Lib

(* let token_list_to_string l = [%show: token list] l *)

let run_compiler_chain prog =
  prog
  |> Parser.parse_program
  |> Desugarar.desugar_program
  |> Cps.cps_program
  |> AlphaConverter.alpha_convert_program
  |> FreevarTransformer.freevar_transfom_program
  |> Grammar.CoreGrammar.show_core_prog

let () =
  let open Base.Sys in
  let args = get_argv () in
  (* We have a file if this is true *)
  if Array.length args > 1 then
    match args with
    | [| _; _filename |] -> Stdlib.print_endline "TODO"
    | _ -> failwith "Expected form: filename.flax"
  else
    while true do
      Stdlib.print_string "> ";
      Stdlib.flush stdout;
      match Stdlib.input_line stdin with
      | "exit" -> exit 0
      | line -> run_compiler_chain line |> Stdlib.print_endline
    done
