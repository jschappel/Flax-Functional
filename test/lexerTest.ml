open OUnit2
open Lib
let operators _ = assert_equal
  ~printer:token_list_to_string
  (lexProgram "+ - * / ( ) = \t \n +")
  [
    Token(PLUS, 1);
    Token(MINUS, 1);
    Token(STAR, 1);
    Token(SLASH, 1);
    Token(LEFT_PAREN, 1); 
    Token(RIGHT_PAREN, 1);
    Token(EQUAL, 1);
    Token(PLUS, 2)
  ]

let numbers _ = assert_equal 
  ~printer:token_list_to_string
  (lexProgram "1 10 100.123")
  [
    Token(NUMBER("1"), 1);
    Token(NUMBER("10"), 1);
    Token(NUMBER("100.123"), 1)
  ]

(* let strings _ = assert_equal
  ~printer:token_list_to_string
  (lexProgram "\"String\"")
  [
    Token("String", STRING, 1);
  ] *)

let reserved_identifiers _ = assert_equal
  ~printer:token_list_to_string
  (lexProgram "fun let in")
  [
    Token(FUN, 1);
    Token(LET, 1);
    Token(IN, 1)
  ]

let identifiers _ = assert_equal
  ~printer:token_list_to_string
  (lexProgram "x xs someValue")
  [
    Token(IDENTIFIER("x"), 1);
    Token(IDENTIFIER("xs"), 1);
    Token(IDENTIFIER("someValue"), 1)
  ]

let suite =
  "Tokens" >:::
   ["Operator Tokens" >:: operators;
      "Numbers" >:: numbers;
      "Reserved Identifiers" >:: reserved_identifiers;
      "Identifiers" >:: identifiers]
  ;;
  
  let () =
    run_test_tt_main suite
  ;;