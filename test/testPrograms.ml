(* open OUnit2 open Lib

   let run_file (path : string) = let open Stdio in In_channel.with_file path ~f:(fun file
   -> In_channel.input_lines file |> lex_program |> parse_expression |>
   interperet_program)

   let factorial _ = let actual = run_file "./programs/fact.flax" in assert_equal
   ~printer:Environment.show_value (NumVal 120.0) actual

   let suite = "AST" >::: [ "factorial" >:: factorial ] let () = run_test_tt_main suite *)
