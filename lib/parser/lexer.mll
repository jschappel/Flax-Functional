{
  open Lexing
  open Parser

  exception SyntaxError of string

  let advance_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    let pos' = { pos with
      pos_bol = lexbuf.lex_curr_pos;
      pos_lnum = pos.pos_lnum + 1
    } in
    lexbuf.lex_curr_p <- pos'
}

(* helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let sign = ['+' '-' '*' '/' '=' '$']

(* regex for a number *)
let frac = '.' digit*
let number = digit* frac?

(* regex for a identifier *)
let id_syms = ['_' '-' '?']
let identifier = (alpha|sign) (alpha|digit|id_syms|sign)*


(* throw away characters *)
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"


rule read_token = 
  parse
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "define"    { DEFINE }
  | "lambda"    { LAMBDA }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "and"       { AND }
  | "not"       { NOT }
  | "list"      { LIST }
  | "vector"    { VECTOR }
  | "or"        { OR }
  | "set!"      { SET }
  | "begin"     { BEGIN }
  | "if"        { IF }
  | "else"      { ELSE }
  | "cond"      { COND }
  | '"'         { read_string (Buffer.create 17) lexbuf }
  | "'"         { read_symbol (Buffer.create 17) lexbuf }
  | white       { read_token lexbuf }
  | newline     { read_token lexbuf }
  | number      { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
  | identifier  { ID (Lexing.lexeme lexbuf) }
  | eof         { EOF }
  | _           { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_symbol buf =
  parse
  | alpha | digit | id_syms | number
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_symbol buf lexbuf
    }
  | '-'
  | white { SYMBOL (Buffer.contents buf) }
  | eof   { SYMBOL (Buffer.contents buf) }
  | _     { SYMBOL (Buffer.contents buf) }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }