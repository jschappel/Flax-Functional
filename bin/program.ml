open Stdio
open Lib

let run_env line =
  value_of @@ parse_expression @@ lexProgram line 

let () = 
  while true do
    Out_channel.printf "> "; Out_channel.flush stdout;
    match In_channel.input_line stdin with
      | Some("exit") -> exit 0
      | Some(line) -> 
        Out_channel.print_endline @@ val_to_string @@ run_env line EmptyEnv;
      | None -> Out_channel.print_endline "NULL"
  done
