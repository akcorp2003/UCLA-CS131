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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
exception ConstTypeParseError

let print_productions = false
let print p = if print_productions then print_string (p^"\n") else ()
# 40 "parser.ml"
let yytransl_const = [|
  261 (* FUNCTION *);
  262 (* PIPE *);
  263 (* WILDCARD *);
  264 (* FN_ARROW *);
  265 (* IF *);
  266 (* THEN *);
  267 (* ELSE *);
  268 (* LET *);
  269 (* REC *);
  270 (* EQ *);
  271 (* IN *);
  272 (* MATCH *);
  273 (* WITH *);
  274 (* LBRACK *);
  275 (* RBRACK *);
  276 (* SEMICOLON *);
  277 (* LPAREN *);
  278 (* RPAREN *);
  279 (* COMMA *);
  280 (* PLUS *);
  281 (* MINUS *);
  282 (* TIMES *);
  283 (* GT *);
  284 (* COLON *);
  285 (* EOD *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* VAR *);
  260 (* CONS *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\005\000\005\000\005\000\007\000\007\000\007\000\008\000\008\000\
\009\000\009\000\010\000\010\000\011\000\011\000\011\000\011\000\
\011\000\011\000\012\000\012\000\012\000\014\000\014\000\013\000\
\013\000\013\000\006\000\006\000\015\000\015\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\016\000\
\016\000\016\000\017\000\017\000\000\000"

let yylen = "\002\000\
\002\000\001\000\004\000\006\000\001\000\004\000\004\000\006\000\
\001\000\003\000\003\000\001\000\003\000\003\000\001\000\003\000\
\001\000\002\000\001\000\002\000\001\000\001\000\003\000\002\000\
\003\000\003\000\001\000\001\000\001\000\003\000\003\000\000\000\
\001\000\003\000\003\000\004\000\004\000\005\000\001\000\002\000\
\002\000\001\000\001\000\003\000\002\000\003\000\003\000\000\000\
\001\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\027\000\028\000\022\000\029\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\053\000\000\000\002\000\
\000\000\000\000\000\000\015\000\000\000\019\000\021\000\043\000\
\000\000\042\000\000\000\000\000\000\000\000\000\039\000\000\000\
\000\000\000\000\000\000\000\000\000\000\024\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\020\000\
\041\000\000\000\000\000\045\000\000\000\000\000\040\000\000\000\
\000\000\000\000\000\000\000\000\000\000\023\000\026\000\000\000\
\025\000\000\000\000\000\000\000\000\000\016\000\000\000\044\000\
\047\000\000\000\046\000\006\000\000\000\003\000\000\000\000\000\
\007\000\034\000\000\000\031\000\050\000\000\000\052\000\000\000\
\000\000\000\000\008\000\004\000\000\000\000\000\036\000\000\000\
\000\000\000\000\038\000"

let yydgoto = "\002\000\
\014\000\015\000\036\000\050\000\017\000\081\000\018\000\019\000\
\020\000\021\000\022\000\023\000\037\000\040\000\095\000\051\000\
\054\000"

let yysindex = "\019\000\
\033\255\000\000\000\000\000\000\000\000\000\000\132\255\099\255\
\002\255\099\255\099\255\089\255\065\255\000\000\233\254\000\000\
\252\254\245\254\254\254\000\000\065\255\000\000\000\000\000\000\
\132\255\000\000\132\255\124\255\021\255\019\255\000\000\034\255\
\029\255\043\255\035\255\030\255\040\255\000\000\250\254\031\255\
\065\255\000\000\000\255\000\255\000\255\000\255\000\255\000\000\
\000\000\042\255\052\255\000\000\009\255\051\255\000\000\099\255\
\099\255\099\255\132\255\132\255\099\255\000\000\000\000\099\255\
\000\000\245\254\245\254\254\254\254\254\000\000\132\255\000\000\
\000\000\132\255\000\000\000\000\063\255\000\000\064\255\069\255\
\000\000\000\000\057\255\000\000\000\000\074\255\000\000\099\255\
\099\255\099\255\000\000\000\000\100\255\132\255\000\000\101\255\
\099\255\100\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\093\255\000\000\000\000\000\000\000\000\000\000\
\044\000\245\255\185\255\000\000\141\255\000\000\000\000\000\000\
\062\255\000\000\094\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\097\255\000\000\000\000\000\000\000\000\
\163\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\102\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\093\255\000\000\000\000\000\000\
\000\000\009\000\029\000\205\255\225\255\000\000\094\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\096\255\000\000\000\000\107\255\000\000\000\000\
\000\000\000\000\000\000\000\000\058\000\000\000\000\000\000\000\
\000\000\072\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\255\255\005\000\000\000\000\000\253\255\002\000\
\075\000\106\000\254\255\001\000\062\000\066\000\034\000\067\000\
\063\000"

let yytablesize = 357
let yytable = "\016\000\
\003\000\004\000\005\000\006\000\033\000\042\000\032\000\031\000\
\035\000\043\000\039\000\030\000\045\000\046\000\034\000\063\000\
\064\000\011\000\048\000\001\000\012\000\055\000\044\000\047\000\
\013\000\031\000\056\000\031\000\031\000\049\000\073\000\074\000\
\053\000\003\000\004\000\005\000\006\000\007\000\048\000\066\000\
\067\000\008\000\058\000\057\000\009\000\059\000\068\000\069\000\
\010\000\061\000\011\000\060\000\065\000\012\000\076\000\077\000\
\078\000\013\000\062\000\031\000\031\000\071\000\083\000\079\000\
\080\000\003\000\004\000\005\000\006\000\029\000\072\000\031\000\
\075\000\088\000\031\000\029\000\090\000\089\000\086\000\064\000\
\029\000\029\000\011\000\029\000\029\000\012\000\091\000\092\000\
\093\000\003\000\004\000\005\000\006\000\007\000\031\000\098\000\
\074\000\008\000\096\000\003\000\004\000\005\000\006\000\007\000\
\010\000\094\000\011\000\008\000\097\000\012\000\038\000\032\000\
\048\000\013\000\010\000\033\000\011\000\030\000\041\000\012\000\
\049\000\070\000\082\000\013\000\003\000\004\000\024\000\025\000\
\051\000\084\000\026\000\099\000\003\000\004\000\024\000\025\000\
\087\000\085\000\026\000\000\000\000\000\027\000\000\000\000\000\
\028\000\052\000\017\000\000\000\029\000\027\000\017\000\017\000\
\028\000\000\000\017\000\000\000\029\000\017\000\000\000\017\000\
\017\000\000\000\017\000\017\000\017\000\017\000\017\000\017\000\
\018\000\017\000\000\000\000\000\018\000\018\000\000\000\000\000\
\018\000\000\000\000\000\018\000\000\000\018\000\018\000\000\000\
\018\000\018\000\018\000\018\000\018\000\018\000\012\000\018\000\
\000\000\000\000\012\000\012\000\000\000\000\000\012\000\000\000\
\000\000\012\000\000\000\012\000\012\000\000\000\012\000\012\000\
\012\000\012\000\013\000\012\000\000\000\012\000\013\000\013\000\
\000\000\000\000\013\000\000\000\000\000\013\000\000\000\013\000\
\013\000\000\000\013\000\013\000\013\000\013\000\014\000\013\000\
\000\000\013\000\014\000\014\000\000\000\000\000\014\000\000\000\
\000\000\014\000\000\000\014\000\014\000\000\000\014\000\014\000\
\014\000\014\000\009\000\014\000\000\000\014\000\009\000\009\000\
\000\000\000\000\009\000\000\000\000\000\009\000\000\000\009\000\
\009\000\000\000\009\000\009\000\000\000\000\000\010\000\009\000\
\000\000\009\000\010\000\010\000\000\000\000\000\010\000\000\000\
\000\000\010\000\000\000\010\000\010\000\000\000\010\000\010\000\
\000\000\000\000\011\000\010\000\000\000\010\000\011\000\011\000\
\000\000\000\000\011\000\000\000\000\000\011\000\000\000\011\000\
\011\000\005\000\011\000\011\000\000\000\005\000\005\000\011\000\
\000\000\011\000\000\000\000\000\005\000\000\000\005\000\005\000\
\000\000\005\000\005\000\035\000\035\000\000\000\000\000\000\000\
\005\000\000\000\035\000\000\000\035\000\035\000\000\000\035\000\
\035\000\037\000\037\000\000\000\000\000\000\000\035\000\000\000\
\037\000\000\000\037\000\037\000\000\000\037\000\037\000\000\000\
\000\000\000\000\000\000\000\000\037\000"

let yycheck = "\001\000\
\001\001\002\001\003\001\004\001\003\001\029\001\008\000\007\000\
\010\000\014\001\012\000\007\000\024\001\025\001\013\001\022\001\
\023\001\018\001\021\000\001\000\021\001\001\001\027\001\026\001\
\025\001\025\000\008\001\027\000\028\000\025\000\022\001\023\001\
\028\000\001\001\002\001\003\001\004\001\005\001\041\000\043\000\
\044\000\009\001\014\001\010\001\012\001\003\001\045\000\046\000\
\016\001\020\001\018\001\017\001\022\001\021\001\056\000\057\000\
\058\000\025\001\019\001\059\000\060\000\020\001\064\000\059\000\
\060\000\001\001\002\001\003\001\004\001\008\001\019\001\071\000\
\022\001\011\001\074\000\014\001\008\001\014\001\074\000\023\001\
\019\001\020\001\018\001\022\001\023\001\021\001\088\000\089\000\
\090\000\001\001\002\001\003\001\004\001\005\001\094\000\097\000\
\023\001\009\001\094\000\001\001\002\001\003\001\004\001\005\001\
\016\001\006\001\018\001\009\001\008\001\021\001\022\001\019\001\
\019\001\025\001\016\001\019\001\018\001\022\001\013\000\021\001\
\019\001\047\000\061\000\025\001\001\001\002\001\003\001\004\001\
\022\001\064\000\007\001\098\000\001\001\002\001\003\001\004\001\
\074\000\071\000\007\001\255\255\255\255\018\001\255\255\255\255\
\021\001\022\001\006\001\255\255\025\001\018\001\010\001\011\001\
\021\001\255\255\014\001\255\255\025\001\017\001\255\255\019\001\
\020\001\255\255\022\001\023\001\024\001\025\001\026\001\027\001\
\006\001\029\001\255\255\255\255\010\001\011\001\255\255\255\255\
\014\001\255\255\255\255\017\001\255\255\019\001\020\001\255\255\
\022\001\023\001\024\001\025\001\026\001\027\001\006\001\029\001\
\255\255\255\255\010\001\011\001\255\255\255\255\014\001\255\255\
\255\255\017\001\255\255\019\001\020\001\255\255\022\001\023\001\
\024\001\025\001\006\001\027\001\255\255\029\001\010\001\011\001\
\255\255\255\255\014\001\255\255\255\255\017\001\255\255\019\001\
\020\001\255\255\022\001\023\001\024\001\025\001\006\001\027\001\
\255\255\029\001\010\001\011\001\255\255\255\255\014\001\255\255\
\255\255\017\001\255\255\019\001\020\001\255\255\022\001\023\001\
\024\001\025\001\006\001\027\001\255\255\029\001\010\001\011\001\
\255\255\255\255\014\001\255\255\255\255\017\001\255\255\019\001\
\020\001\255\255\022\001\023\001\255\255\255\255\006\001\027\001\
\255\255\029\001\010\001\011\001\255\255\255\255\014\001\255\255\
\255\255\017\001\255\255\019\001\020\001\255\255\022\001\023\001\
\255\255\255\255\006\001\027\001\255\255\029\001\010\001\011\001\
\255\255\255\255\014\001\255\255\255\255\017\001\255\255\019\001\
\020\001\006\001\022\001\023\001\255\255\010\001\011\001\027\001\
\255\255\029\001\255\255\255\255\017\001\255\255\019\001\020\001\
\255\255\022\001\023\001\010\001\011\001\255\255\255\255\255\255\
\029\001\255\255\017\001\255\255\019\001\020\001\255\255\022\001\
\023\001\010\001\011\001\255\255\255\255\255\255\029\001\255\255\
\017\001\255\255\019\001\020\001\255\255\022\001\023\001\255\255\
\255\255\255\255\255\255\255\255\029\001"

let yynames_const = "\
  FUNCTION\000\
  PIPE\000\
  WILDCARD\000\
  FN_ARROW\000\
  IF\000\
  THEN\000\
  ELSE\000\
  LET\000\
  REC\000\
  EQ\000\
  IN\000\
  MATCH\000\
  WITH\000\
  LBRACK\000\
  RBRACK\000\
  SEMICOLON\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  GT\000\
  COLON\000\
  EOD\000\
  "

let yynames_block = "\
  INT\000\
  BOOL\000\
  VAR\000\
  CONS\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    Obj.repr(
# 34 "parser.mly"
           ( print ";;"; _1 )
# 286 "parser.ml"
               : modecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 37 "parser.mly"
        ( print "d -> e"; Expr(_1) )
# 293 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 38 "parser.mly"
                   ( print "d -> let x = e"; Let(_2,_4) )
# 301 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 40 "parser.mly"
      ( print "d -> let rec f p = e"; LetRec(_3,Function(_4,_6)) )
# 310 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compareexp) in
    Obj.repr(
# 44 "parser.mly"
               ( _1 )
# 317 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 45 "parser.mly"
                                  ( print "e -> function p -> e"; Function(_2,_4) )
# 325 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'fn_patterns) in
    Obj.repr(
# 46 "parser.mly"
                               ( print "e -> match e with ps"; Match(_2,_4) )
# 333 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 47 "parser.mly"
                             ( print "e -> if e then e else e"; If(_2,_4,_6) )
# 342 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'plusexp) in
    Obj.repr(
# 51 "parser.mly"
           ( _1 )
# 349 "parser.ml"
               : 'compareexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compareexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'plusexp) in
    Obj.repr(
# 52 "parser.mly"
                          ( BinOp(_1,Eq,_3) )
# 357 "parser.ml"
               : 'compareexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compareexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'plusexp) in
    Obj.repr(
# 53 "parser.mly"
                          ( BinOp(_1,Gt,_3) )
# 365 "parser.ml"
               : 'compareexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'timesexp) in
    Obj.repr(
# 56 "parser.mly"
             ( _1 )
# 372 "parser.ml"
               : 'plusexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'plusexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'timesexp) in
    Obj.repr(
# 57 "parser.mly"
                          ( BinOp(_1,Plus,_3) )
# 380 "parser.ml"
               : 'plusexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'plusexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'timesexp) in
    Obj.repr(
# 58 "parser.mly"
                           ( BinOp(_1,Minus,_3) )
# 388 "parser.ml"
               : 'plusexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'negexp) in
    Obj.repr(
# 62 "parser.mly"
           ( _1 )
# 395 "parser.ml"
               : 'timesexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'timesexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'negexp) in
    Obj.repr(
# 63 "parser.mly"
                          ( BinOp(_1,Times,_3) )
# 403 "parser.ml"
               : 'timesexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appexp) in
    Obj.repr(
# 67 "parser.mly"
           ( _1 )
# 410 "parser.ml"
               : 'negexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'appexp) in
    Obj.repr(
# 68 "parser.mly"
                 ( Negate(_2) )
# 417 "parser.ml"
               : 'negexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'baseexp) in
    Obj.repr(
# 72 "parser.mly"
            ( print "ae -> be"; _1 )
# 424 "parser.ml"
               : 'appexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'baseexp) in
    Obj.repr(
# 73 "parser.mly"
                     (
               match _1 with
                 Data(c,None) -> Data(c,Some _2)
                | _ -> FunctionCall(_1,_2)
             )
# 436 "parser.ml"
               : 'appexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 80 "parser.mly"
        ( print "be -> c"; _1 )
# 443 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "parser.mly"
          ( print ("be -> var "^_1); Var(_1) )
# 450 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'explist) in
    Obj.repr(
# 82 "parser.mly"
                          ( _2 )
# 457 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
                  ( Tuple [] )
# 463 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exptuple) in
    Obj.repr(
# 84 "parser.mly"
                           ( Tuple _2 )
# 470 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 85 "parser.mly"
                      ( print "be -> (e)"; _2 )
# 477 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 89 "parser.mly"
         ( print ("c -> int "^(string_of_int _1)); IntConst(_1) )
# 484 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 90 "parser.mly"
         ( print ("c -> bool "^(string_of_bool _1)); BoolConst(_1) )
# 491 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "parser.mly"
           ( Data(_1, None) )
# 498 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 95 "parser.mly"
                ( [_1; _3] )
# 506 "parser.ml"
               : 'exptuple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exptuple) in
    Obj.repr(
# 96 "parser.mly"
                     ( _1 :: _3 )
# 514 "parser.ml"
               : 'exptuple))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
  ( Data("Nil", None) )
# 520 "parser.ml"
               : 'explist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 101 "parser.mly"
      ( Data("Cons", Some (Tuple [_1 ; Data("Nil", None)])) )
# 527 "parser.ml"
               : 'explist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'explist) in
    Obj.repr(
# 102 "parser.mly"
                        ( Data("Cons", Some (Tuple [_1 ; _3])) )
# 535 "parser.ml"
               : 'explist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 106 "parser.mly"
                         ( print "fp -> p -> e"; [(_1,_3)] )
# 543 "parser.ml"
               : 'fn_patterns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'fn_patternsAux) in
    Obj.repr(
# 107 "parser.mly"
                                        ( print "fp -> e fpA"; (_1,_3) :: _4 )
# 552 "parser.ml"
               : 'fn_patterns))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 111 "parser.mly"
                              ( print "fpA -> | p -> e"; [(_2,_4)] )
# 560 "parser.ml"
               : 'fn_patternsAux))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'fn_patternsAux) in
    Obj.repr(
# 112 "parser.mly"
                                             ( print "fpA -> | p -> e fpA"; (_2,_4) :: _5 )
# 569 "parser.ml"
               : 'fn_patternsAux))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 116 "parser.mly"
          ( print "p -> c";
				match _1 with
					 IntConst(i)  -> IntPat(i)
				  | BoolConst(b) -> BoolPat(b)
				  | Data(c, None) -> DataPat(c, None)
				  | _ -> raise ConstTypeParseError )
# 581 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 122 "parser.mly"
                 ( print ("p -> -int "^(string_of_int (_2 * -1))); IntPat(_2 * -1) )
# 588 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 123 "parser.mly"
                   ( DataPat(_1, Some _2) )
# 596 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "parser.mly"
               ( print "p -> _"; WildcardPat )
# 602 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 125 "parser.mly"
          ( print ("p -> var "^_1^":t"); VarPat(_1) )
# 609 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'patlist) in
    Obj.repr(
# 126 "parser.mly"
                            ( _2 )
# 616 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "parser.mly"
                    ( TuplePat [] )
# 622 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pattuple) in
    Obj.repr(
# 128 "parser.mly"
                             ( TuplePat _2 )
# 629 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    Obj.repr(
# 129 "parser.mly"
                            ( print "p -> (p)"; _2 )
# 636 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "parser.mly"
   ( DataPat("Nil", None) )
# 642 "parser.ml"
               : 'patlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 134 "parser.mly"
          ( DataPat("Cons", Some (TuplePat [_1 ; DataPat("Nil", None)])) )
# 649 "parser.ml"
               : 'patlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'patlist) in
    Obj.repr(
# 135 "parser.mly"
                            ( DataPat("Cons", Some (TuplePat [_1 ; _3])) )
# 657 "parser.ml"
               : 'patlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 139 "parser.mly"
                        ( [_1; _3] )
# 665 "parser.ml"
               : 'pattuple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattuple) in
    Obj.repr(
# 140 "parser.mly"
                         ( _1 :: _3 )
# 673 "parser.ml"
               : 'pattuple))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : modecl)
