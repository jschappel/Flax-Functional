open Core
open Token

exception LexError of string * int

type line = string
type program = line list

(* Reserved Identifiers *)
let map =
  Map.of_alist_exn
    (module String)
    [
      ("let", LET);
      ("in", IN);
      ("fn", FUN);
      ("if", IF);
      ("then", THEN);
      ("else", ELSE);
      ("true", BOOL true);
      ("and", AND);
      ("or", OR);
      ("not", NOT);
      ("false", BOOL false);
      ("letrec", LETREC);
    ]

let lex_number (txt : char list) (line : int) : string =
  let rec helper txt seen_decimal acc =
    match txt with
    | [] -> acc
    | x :: xs -> (
        match x with
        | '0' .. '9' -> helper xs seen_decimal @@ acc ^ Char.to_string x
        | '.' when equal_bool seen_decimal false ->
            helper xs true @@ acc ^ Char.to_string x
        | '.' -> raise @@ LexError ("Invlid number. To many '.'", line)
        | _ ->
            if String.equal "" acc then
              raise @@ LexError ("Invlid number.", line)
            else acc)
  in
  helper txt false ""

let lex_identifier (txt : char list) : int * tokenType =
  let ident =
    String.of_char_list @@ List.take_while txt ~f:(fun x -> Char.is_alphanum x)
  in
  let len = String.length ident in
  match Map.find map ident with
  | Some keyword -> (len, keyword)
  | None -> (len, IDENTIFIER ident)

let lex_string (txt : char list) : string =
  String.of_char_list
  @@ List.take_while txt ~f:(fun c -> not (Char.equal c '"'))

let lex_minus (txt : char list) (line : int) : tokenType * int =
  match txt with
  | '-' :: y :: xs when Char.is_digit y ->
      let num_str = "-" ^ lex_number (y :: xs) line in
      (NUMBER (float_of_string num_str), String.length num_str)
  | '-' :: _ -> (MINUS, 1)
  | _ -> raise @@ LexError ("Unreachable", line)

let lex_line ?line_num txt =
  let line = match line_num with None -> 1 | Some l -> l in
  let rec lex_line_aux txt line =
    match txt with
    | [] -> []
    | x :: xs -> (
        match x with
        | '(' -> Token (LEFT_PAREN, line) :: lex_line_aux xs line
        | ')' -> Token (RIGHT_PAREN, line) :: lex_line_aux xs line
        | '.' -> Token (DOT, line) :: lex_line_aux xs line
        | '*' -> Token (STAR, line) :: lex_line_aux xs line
        | '+' -> Token (PLUS, line) :: lex_line_aux xs line
        | ',' -> Token (COMMA, line) :: lex_line_aux xs line
        | ';' -> Token (SEMICOLON, line) :: lex_line_aux xs line
        | '-' ->
            let tt, size = lex_minus txt line in
            let xs = List.drop txt size in
            Token (tt, line) :: lex_line_aux xs line
        | '/' -> Token (SLASH, line) :: lex_line_aux xs line
        | '=' when equal_option Char.equal (List.hd xs) (Some '>') ->
            Token (ARROW, line) :: lex_line_aux (List.drop xs 1) line
        | '=' when equal_option Char.equal (List.hd xs) (Some '=') ->
            Token (EQ_EQ, line) :: lex_line_aux (List.drop xs 1) line
        | '=' -> Token (EQ, line) :: lex_line_aux xs line
        | '>' when equal_option Char.equal (List.hd xs) (Some '=') ->
            Token (GT_EQ, line) :: lex_line_aux (List.drop xs 1) line
        | '>' -> Token (GT, line) :: lex_line_aux xs line
        | '<' when equal_option Char.equal (List.hd xs) (Some '=') ->
            Token (LT_EQ, line) :: lex_line_aux (List.drop xs 1) line
        | '<' -> Token (LT, line) :: lex_line_aux xs line
        | '!' when equal_option Char.equal (List.hd xs) (Some '=') ->
            Token (NOT_EQ, line) :: lex_line_aux (List.drop xs 1) line
        | '\n' -> lex_line_aux xs @@ (line + 1)
        | '"' ->
            let s = lex_string xs in
            let xs = List.drop xs @@ (1 + String.length s) in
            Token (STRING s, line) :: lex_line_aux xs line
        | ' ' | '\t' -> lex_line_aux xs line
        | '0' .. '9' ->
            let num_str = lex_number txt line in
            let xs = List.drop txt @@ String.length num_str in
            Token (NUMBER (float_of_string num_str), line)
            :: lex_line_aux xs line
        | 'a' .. 'z' | 'A' .. 'Z' ->
            let len, tt = lex_identifier txt in
            let xs = List.drop txt len in
            Token (tt, line) :: lex_line_aux xs line
        | _ ->
            let msg = Printf.sprintf "Invalid token %c" x in
            raise @@ LexError (msg, line))
  in

  lex_line_aux (String.to_list txt) line

let lex_program prog =
  let rec lex_program_aux (line : int) = function
    | [] -> []
    | x :: xs -> lex_line ~line_num:line x @ lex_program_aux (line + 1) xs
  in
  lex_program_aux 1 prog
