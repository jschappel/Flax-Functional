open OUnit2
open Lib
open Base

let token_list_to_string l =
  List.fold_right ~f:(fun x a -> a ^ show_token x ^ " ") l ~init:"["

let operators _ =
  assert_equal ~printer:token_list_to_string
    (lex_line "+ - * / ( ) , ; = => == > < >= <= != \t \n +")
    [
      Token (PLUS, 1);
      Token (MINUS, 1);
      Token (STAR, 1);
      Token (SLASH, 1);
      Token (LEFT_PAREN, 1);
      Token (RIGHT_PAREN, 1);
      Token (COMMA, 1);
      Token (SEMICOLON, 1);
      Token (EQ, 1);
      Token (ARROW, 1);
      Token (EQ_EQ, 1);
      Token (GT, 1);
      Token (LT, 1);
      Token (GT_EQ, 1);
      Token (LT_EQ, 1);
      Token (NOT_EQ, 1);
      Token (PLUS, 2);
    ]

let numbers _ =
  assert_equal ~printer:token_list_to_string (lex_line "1 10 100.123")
    [ Token (NUMBER 1.0, 1); Token (NUMBER 10.0, 1); Token (NUMBER 100.123, 1) ]

let negative_numbers _ =
  assert_equal ~printer:token_list_to_string (lex_line "-10 -10.0")
    [ Token (NUMBER (-10.0), 1); Token (NUMBER (-10.0), 1) ]

let strings _ =
  assert_equal ~printer:token_list_to_string
    (lex_line "\"String\" \"josh\"")
    [ Token (STRING "String", 1); Token (STRING "josh", 1) ]

let reserved_identifiers _ =
  assert_equal ~printer:token_list_to_string
    (lex_line "fn let letrec in if then else and or true false not")
    [
      Token (FUN, 1);
      Token (LET, 1);
      Token (LETREC, 1);
      Token (IN, 1);
      Token (IF, 1);
      Token (THEN, 1);
      Token (ELSE, 1);
      Token (AND, 1);
      Token (OR, 1);
      Token (BOOL true, 1);
      Token (BOOL false, 1);
      Token (NOT, 1);
    ]

let identifiers _ =
  assert_equal ~printer:token_list_to_string
    (lex_line "x xs someValue")
    [
      Token (IDENTIFIER "x", 1);
      Token (IDENTIFIER "xs", 1);
      Token (IDENTIFIER "someValue", 1);
    ]

let lex_letrec _ =
  assert_equal ~printer:token_list_to_string
    [
      Token (LETREC, 1);
      Token (IDENTIFIER "add", 2);
      Token (LEFT_PAREN, 2);
      Token (IDENTIFIER "x", 2);
      Token (COMMA, 2);
      Token (IDENTIFIER "y", 2);
      Token (RIGHT_PAREN, 2);
      Token (ARROW, 2);
      Token (IDENTIFIER "x", 2);
      Token (PLUS, 2);
      Token (IDENTIFIER "y", 2);
      Token (SEMICOLON, 2);
      Token (IDENTIFIER "sub", 3);
      Token (LEFT_PAREN, 3);
      Token (IDENTIFIER "xx", 3);
      Token (COMMA, 3);
      Token (IDENTIFIER "yy", 3);
      Token (RIGHT_PAREN, 3);
      Token (ARROW, 3);
      Token (IDENTIFIER "xx", 3);
      Token (MINUS, 3);
      Token (IDENTIFIER "yy", 3);
      Token (IN, 4);
      Token (IDENTIFIER "add", 5);
      Token (LEFT_PAREN, 5);
      Token (NUMBER 4.0, 5);
      Token (COMMA, 5);
      Token (NUMBER 5.0, 5);
      Token (RIGHT_PAREN, 5);
    ]
    (lex_line
       "letrec\n\
       \        add(x, y) => x + y;\n\
       \        sub(xx, yy) => xx - yy\n\
       \      in\n\
       \       add(4,5)")

let suite =
  "Tokens"
  >::: [
         "Operator Tokens" >:: operators;
         "Numbers" >:: numbers;
         "Negative Numbers" >:: negative_numbers;
         "Strings" >:: strings;
         "Reserved Identifiers" >:: reserved_identifiers;
         "Identifiers" >:: identifiers;
         "Others" >:: lex_letrec;
       ]

let () = run_test_tt_main suite
