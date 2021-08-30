open Stdio
open Lib

let run_env line =
  lexProgram line 

let () = 
  let interpret_input = lexProgram in
  while true do
    Out_channel.printf "> "; Out_channel.flush stdout;
    match In_channel.input_line stdin with
      | Some("exit") -> exit 0
      | Some(line) -> 
        Out_channel.print_endline @@ token_list_to_string @@ interpret_input line;
      | None -> Out_channel.print_endline "NULL"
  done
