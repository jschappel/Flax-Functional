open Core

type tokenType = 
    LEFT_PAREN 
    | RIGHT_PAREN 
    | EQUAL
    | PLUS 
    | MINUS 
    | STAR 
    | SLASH 
    | DOT
    | LET 
    | IN 
    | FUN
    | IDENTIFIER of string
    | NUMBER of float

type token = Token of tokenType * int

exception LexError of string * int

(* Reserved Identifiers *)
let map = Map.of_alist_exn (module String) [
  ("let", LET);
  ("in", IN);
  ("fun", FUN)]


let lex_number (txt: char list) (line: int): float =
  let rec helper txt seen_decimal acc =
    match txt with
      | [] -> float_of_string acc
      | x::xs ->
      match x with
        | '0'..'9' -> helper xs seen_decimal @@ acc ^ Char.to_string x 
        | '.' when equal_bool seen_decimal false -> helper xs true @@ acc ^ Char.to_string x
        | '.' -> raise @@ LexError("Invlid number. To many '.'", line)
        | _ -> if "" == acc then raise @@ LexError("Invlid number.", line) else float_of_string acc
  in helper txt false ""

let lex_identifier txt =
  let ident = String.of_char_list @@ List.take_while txt ~f:(fun x -> Char.is_alphanum x) in
  match (Map.find map ident) with
    | Some(keyword) -> keyword
    | None -> IDENTIFIER(ident)

let lexProgram prog =
  let rec lexLine txt line =
    match txt with 
      | [] -> []
      | (x::xs) ->
      match x with 
        | '(' -> Token(LEFT_PAREN, line) :: lexLine xs line
        | ')' -> Token(RIGHT_PAREN, line) :: lexLine xs line 
        | '.' -> Token(DOT, line) :: lexLine xs line
        | '*' -> Token(STAR, line) :: lexLine xs line
        | '+' -> Token(PLUS, line) :: lexLine xs line
        | '-' -> Token(MINUS, line) :: lexLine xs line
        | '/' -> Token(SLASH, line) :: lexLine xs line
        | '=' -> Token(EQUAL, line) :: lexLine xs line
        | '\n' -> lexLine xs @@ line + 1
        | ' '| '\t' -> lexLine xs line
        | '0'..'9' -> let num = lex_number txt line in
                      let xs = List.drop txt @@ String.length num in
                      Token(NUMBER(num), line) :: lexLine xs line
        | 'a'..'z'
        | 'A'..'Z' -> let tt = lex_identifier txt in
                      let xs = List.drop txt @@ String.length i in
                      Token(tt, line) :: lexLine xs line
        | _ -> let msg = Printf.sprintf "Invalid token %c" x in
                raise @@ LexError(msg, line)
  in lexLine (String.to_list prog) 1