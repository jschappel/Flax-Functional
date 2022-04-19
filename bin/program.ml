open Stdio
open Lib

let run_env line =
  try
    Enviroment.show_value
    @@ value_of (parse_expression @@ lexProgram line) EmptyEnv
  with
  | Failure msg -> msg
  | ParseError e -> e
  | LexError (e, _) -> e
  | InterpreterError e -> e

let () =
  while true do
    Out_channel.printf "> ";
    Out_channel.flush stdout;
    match In_channel.input_line stdin with
    | Some "exit" -> exit 0
    | Some line -> Out_channel.print_endline @@ run_env line
    | None -> Out_channel.print_endline "NULL"
  done
