open OUnit2
open Lib

let operators _ = assert_equal
  ~printer:token_list_to_string
  (lexProgram "+ - * / ( ) , = == > < >= <= != \t \n +")
  [
    Token(PLUS, 1);
    Token(MINUS, 1);
    Token(STAR, 1);
    Token(SLASH, 1);
    Token(LEFT_PAREN, 1); 
    Token(RIGHT_PAREN, 1);
    Token(COMMA, 1);
    Token(EQ, 1);
    Token(EQ_EQ, 1);
    Token(GT, 1);
    Token(LT, 1);
    Token(GT_EQ, 1);
    Token(LT_EQ, 1);
    Token(NOT_EQ, 1);
    Token(PLUS, 2)
  ]

let numbers _ = assert_equal 
  ~printer:token_list_to_string
  (lexProgram "1 10 100.123")
  [
    Token(NUMBER(1.0), 1);
    Token(NUMBER(10.0), 1);
    Token(NUMBER(100.123), 1)
  ]

let negative_numbers _ = assert_equal
  ~printer:token_list_to_string
  (lexProgram "-10 -10.0")
  [
    Token(NUMBER(-10.0), 1);
    Token(NUMBER(-10.0), 1);
  ]

let strings _ = assert_equal
  ~printer:token_list_to_string
  (lexProgram "\"String\" \"josh\"")
  [
    Token(STRING("String"), 1);
    Token(STRING("josh"), 1);
  ]

let reserved_identifiers _ = assert_equal
  ~printer:token_list_to_string
  (lexProgram "fn let in if then else and or true false not")
  [
    Token(FUN, 1);
    Token(LET, 1);
    Token(IN, 1);
    Token(IF, 1);
    Token(THEN, 1);
    Token(ELSE, 1);
    Token(AND, 1);
    Token(OR, 1);
    Token(BOOL(true), 1);
    Token(BOOL(false), 1);
    Token(NOT, 1);
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
      "Negative Numbers" >:: negative_numbers;
      "Strings" >:: strings;
      "Reserved Identifiers" >:: reserved_identifiers;
      "Identifiers" >:: identifiers]
  ;;
  
  let () =
    run_test_tt_main suite
  ;;
