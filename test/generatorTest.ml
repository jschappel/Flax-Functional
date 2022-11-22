open OUnit2
open Flax_core.Lib.Utils.Generator

module ParamSymGen = SymGen (struct
  let s = "V"
end)

module ContSymGen = SymGen (struct
  let s = "K"
end)

module FormatterSymGen = SymGenFmt (struct
  let fmt num = "__" ^ "D" ^ Int.to_string num ^ "__"
end)

let generate_param_symbols _ =
  assert_equal (ParamSymGen.gen_sym ()) "V0";
  assert_equal (ParamSymGen.gen_sym ()) "V1";
  assert_equal (ParamSymGen.gen_sym ()) "V2";

  ParamSymGen.reset ();

  assert_equal (ParamSymGen.gen_sym ()) "V0";
  assert_equal (ParamSymGen.gen_sym ()) "V1";
  assert_equal (ParamSymGen.gen_sym ()) "V2"

let generate_cont_symbols _ =
  assert_equal (ContSymGen.gen_sym ()) "K0";
  assert_equal (ContSymGen.gen_sym ()) "K1";
  assert_equal (ContSymGen.gen_sym ()) "K2";

  ContSymGen.reset ();

  assert_equal (ContSymGen.gen_sym ()) "K0";
  assert_equal (ContSymGen.gen_sym ()) "K1";
  assert_equal (ContSymGen.gen_sym ()) "K2"

let generate_fmt_symbols _ =
  assert_equal (FormatterSymGen.gen_sym ()) "__D0__";
  assert_equal (FormatterSymGen.gen_sym ()) "__D1__";
  assert_equal (FormatterSymGen.gen_sym ()) "__D2__";

  FormatterSymGen.reset ();

  assert_equal (FormatterSymGen.gen_sym ()) "__D0__";
  assert_equal (FormatterSymGen.gen_sym ()) "__D1__";
  assert_equal (FormatterSymGen.gen_sym ()) "__D2__"

let suite =
  "Generator tests"
  >::: [
         "Generate Syms" >:: generate_param_symbols;
         "Generate Syms" >:: generate_cont_symbols;
         "Generate Formatterd Syms" >:: generate_fmt_symbols;
       ]

let _ = run_test_tt_main suite
