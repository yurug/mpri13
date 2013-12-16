type token =
  | EXTERNAL
  | INSTANCE
  | BIGRARROW
  | CLASS
  | AS
  | UNDERSCORE
  | WITH
  | MATCH
  | ASSERT
  | FALSE
  | RARROW
  | RPAREN
  | LPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | STAR
  | TCHAR
  | TINT
  | TUNIT
  | DOT
  | SEMICOLON
  | LID of (string)
  | UID of (string)
  | TID of (string)
  | INT of int
  | CHAR of char
  | STRING of string
  | FUN
  | LET
  | IN
  | REC
  | AND
  | EQUAL
  | COLON
  | COMMA
  | TYPE
  | OF
  | PIPE
  | EOF
