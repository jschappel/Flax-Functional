open Stdlib
open Flax_core.Lib

(* let token_list_to_string l = [%show: token list] l *)

(* let run_env line = try lex_line line |> parse_expression |> interperet_program |>
   Environment.show_value with | Failure msg -> msg | ParseError e -> e | LexError (e, _)
   -> e | InterpreterError e -> e

   let run_file (path : string) = In_channel.with_file path ~f:(fun file ->
   In_channel.input_lines file |> lex_program |> parse_expression |>
   Transformations.CPS.value_of_program |> Environment.show_value) *)

let () =
  let open Base.Sys in
  let args = get_argv () in
  (* We have a file if this is true *)
  if Array.length args > 1 then
    match args with
    | [| _; _filename |] -> Stdlib.print_endline "TODO" (* @@ run_file filename *)
    | _ -> failwith "Expected form: filename.flax"
  else
    while true do
      Stdlib.print_string "> ";
      Stdlib.flush stdout;
      match Stdlib.input_line stdin with
      | "exit" -> exit 0
      | line ->
          Build.run_prog line
          |> Build.show_core_prog
          |> Stdlib.print_endline (* @@ run_env line *)
    done
