open Stdio
open Flax_core.Lib

(* let token_list_to_string l = [%show: token list] l *)

(* let run_env line = try lex_line line |> parse_expression |> interperet_program |>
   Enviroment.show_value with | Failure msg -> msg | ParseError e -> e | LexError (e, _)
   -> e | InterpreterError e -> e

   let run_file (path : string) = In_channel.with_file path ~f:(fun file ->
   In_channel.input_lines file |> lex_program |> parse_expression |>
   Transformations.CPS.value_of_program |> Enviroment.show_value) *)

let () =
  let open Base.Sys in
  let args = get_argv () in
  (* We have a file if this is true *)
  if Array.length args > 1 then
    match args with
    | [| _; _filename |] -> Out_channel.print_endline "TODO" (* @@ run_file filename *)
    | _ -> failwith "Expected form: filename.flax"
  else
    while true do
      Out_channel.printf "> ";
      Out_channel.flush stdout;
      match In_channel.input_line stdin with
      | Some "exit" -> exit 0
      | Some line ->
          Parser.parse_program line
          |> Parser.show_program
          |> Out_channel.print_endline (* @@ run_env line *)
      | None -> Out_channel.print_endline "NULL"
    done
