open Token
open Base


let token_list_to_string tokens =
  let fmt n v = Printf.sprintf "(line: %d    Val: %s)" n v in
  let token_to_string _ token =
    match token with
    | Token(PLUS, line)           -> fmt line "PLUS"
    | Token(MINUS, line)          -> fmt line "MINUS"
    | Token(STAR, line)           -> fmt line "STAR"
    | Token(SLASH, line)          -> fmt line "SLASH"
    | Token(LEFT_PAREN, line)     -> fmt line "LEFT_PAREN"
    | Token(RIGHT_PAREN, line)    -> fmt line "RIGHT_PAREN"
    | Token(EQUAL, line)          -> fmt line "EQUAL"
    | Token(NUMBER(n), line)      -> fmt line @@ string_of_float n
    | Token(LET, line)            -> fmt line "LET"
    | Token(FUN, line)            -> fmt line "FUN"
    | Token(IN, line)             -> fmt line "IN"
    | Token(IF, line)             -> fmt line "IF"
    | Token(THEN, line)           -> fmt line "THEN"
    | Token(ELSE, line)           -> fmt line "ELSE"
    | Token(IDENTIFIER(i), line)  -> fmt line i
    | Token(STRING(s), line)      -> fmt line  @@ "\"" ^ s ^ "\""
    | _ -> "Invalid token given" in
  (List.fold tokens ~init:"" ~f:token_to_string)
