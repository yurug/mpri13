(** Lexical analysis. *)

{
  open Tokens

  let lexical_error lexbuf = Errors.fatal [lexbuf.Lexing.lex_curr_p]

  let new_line lexbuf = Lexing.(
    let cpos = lexbuf.lex_curr_p in
    let npos = { cpos with
      pos_lnum = cpos.pos_lnum + 1;
      pos_bol = cpos.pos_cnum;
    }
    in
    lexbuf.lex_curr_p <- npos
  )
}


let newline =
  ('\010' | '\013' | "\013\010")

let blank =
  [' ' '\009' '\012']

let lowercase =
  ['a'-'z' '\223'-'\246' '\248'-'\255' '_']

let uppercase =
  ['A'-'Z' '\192'-'\214' '\216'-'\222']

let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

let identifier =
  lowercase identchar*

let uidentifier =
  uppercase identchar*

rule token = parse

  (** Layout *)

  | newline {
    new_line lexbuf;
    token lexbuf
  }

  | blank+ {
    token lexbuf
  }

  | '"' {
    string (Buffer.create 13) lexbuf
  }

  | "(*"   {
    comment 0 lexbuf
  }

  (** Keywords *)

  | "fun"      { FUN }
  | "let"      { LET }
  | "rec"      { REC }
  | "in"       { IN }
  | "and"      { AND }
  | "of"       { OF }
  | "type"     { TYPE }
  | "unit"     { TUNIT }
  | "int"      { TINT }
  | "char"     { TCHAR }
  | "as"       { AS }
  | "with"     { WITH }
  | "match"    { MATCH }
  | "class"    { CLASS }
  | "instance" { INSTANCE }
  | "external" { EXTERNAL }

  (** Punctuation. *)

  | "_"    { UNDERSCORE }
  | "{"    { LBRACE }
  | "}"    { RBRACE }
  | "("    { LPAREN }
  | ")"    { RPAREN }
  | "["    { LBRACKET }
  | "]"    { RBRACKET }
  | ","    { COMMA }
  | ";"    { SEMICOLON }
  | "."    { DOT }
  | eof    { EOF }

  (** Operators. *)
  | "="    { EQUAL }
  | ":"    { COLON }
  | "|"    { PIPE }
  | "*"    { STAR }
  | "->"   { RARROW }
  | "=>"   { BIGRARROW }

  (** Literals. *)
  | ['0'-'9']+ as i        { INT (int_of_string i) }
  | "'\\''"                { CHAR '\'' }
  | "'" ([^'\''] as c) "'" { CHAR c }

  (** Identifiers. *)

  | identifier as i  { LID i }
  | uidentifier as i { UID i }
  | '\'' identifier as i { TID i }

  | _  {
    lexical_error lexbuf "Invalid character."
  }

and comment level = parse
  | "*)" {
    if level > 0 then
      comment (pred level) lexbuf
    else
      token lexbuf
  }
  | "(*" {
    comment (succ level) lexbuf
  }
  | eof {
    lexical_error lexbuf "Unterminated comment."
  }
  | newline {
    new_line lexbuf;
    comment level lexbuf
  }
  | _ {
    comment level lexbuf
  }

and string buffer = parse
  | '"' {
    STRING (Buffer.contents buffer)
  }
  | eof {
    lexical_error lexbuf "Unterminated string."
  }
  | _ as c {
    Buffer.add_char buffer c;
    string buffer lexbuf
  }
