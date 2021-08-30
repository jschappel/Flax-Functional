open Stdio
open Lib

(* open Lexer *)

let run_env line =
  lexProgram line 


let loop =
  begin
    Out_channel.printf "> ";
    Out_channel.flush stdout ;
    match In_channel.input_line stdin with
      | Some(line) -> Out_channel.print_endline line
      | None -> Out_channel.print_endline "NULL"
  end

let () = loop