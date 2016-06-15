type token =
  | INT of (int)
  | BOOL of (bool)
  | VAR of (string)
  | CONS of (string)
  | FUNCTION
  | PIPE
  | WILDCARD
  | FN_ARROW
  | IF
  | THEN
  | ELSE
  | LET
  | REC
  | EQ
  | IN
  | MATCH
  | WITH
  | LBRACK
  | RBRACK
  | SEMICOLON
  | LPAREN
  | RPAREN
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | GT
  | COLON
  | EOD

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> modecl
