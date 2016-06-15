%{
exception ConstTypeParseError

let print_productions = false
let print p = if print_productions then print_string (p^"\n") else ()
%}

%token <int> INT
%token <bool> BOOL
%token <string> VAR
%token <string> CONS
                  
%token FUNCTION
%token PIPE
%token WILDCARD
%token FN_ARROW
%token IF THEN ELSE
%token LET REC EQ IN
%token MATCH WITH
%token LBRACK RBRACK SEMICOLON
%token LPAREN RPAREN COMMA
%token PLUS MINUS TIMES GT
%token COLON
%token EOD

%right FN_ARROW
%left PLUS
%left PIPE

%start main
%type <modecl> main
%%
main:
  decl EOD { print ";;"; $1 }

decl:
    exp { print "d -> e"; Expr($1) }
|   LET VAR EQ exp { print "d -> let x = e"; Let($2,$4) }
|   LET REC VAR pattern EQ exp
      { print "d -> let rec f p = e"; LetRec($3,Function($4,$6)) }
;

exp:
    compareexp { $1 }
  | FUNCTION pattern FN_ARROW exp { print "e -> function p -> e"; Function($2,$4) }
  | MATCH exp WITH fn_patterns { print "e -> match e with ps"; Match($2,$4) }
  | IF exp THEN exp ELSE exp { print "e -> if e then e else e"; If($2,$4,$6) }
;

compareexp:
   plusexp { $1 }
  | compareexp EQ plusexp { BinOp($1,Eq,$3) }
  | compareexp GT plusexp { BinOp($1,Gt,$3) }
      
plusexp:
    timesexp { $1 }
  | plusexp PLUS timesexp { BinOp($1,Plus,$3) }
  | plusexp MINUS timesexp { BinOp($1,Minus,$3) }
;

timesexp:
    negexp { $1 }
  | timesexp TIMES negexp { BinOp($1,Times,$3) }
;

negexp:
    appexp { $1 }
  | MINUS appexp { Negate($2) }
;

appexp:
    baseexp { print "ae -> be"; $1 }
    | appexp baseexp {
               match $1 with
                 Data(c,None) -> Data(c,Some $2)
                | _ -> FunctionCall($1,$2)
             }

baseexp:      
  const { print "be -> c"; $1 }
  | VAR   { print ("be -> var "^$1); Var($1) }
  | LBRACK explist RBRACK { $2 }
  | LPAREN RPAREN { Tuple [] }
  | LPAREN exptuple RPAREN { Tuple $2 }
  | LPAREN exp RPAREN { print "be -> (e)"; $2 }
;

const:
    INT  { print ("c -> int "^(string_of_int $1)); IntConst($1) }
  | BOOL { print ("c -> bool "^(string_of_bool $1)); BoolConst($1) }
  | CONS   { Data($1, None) }
;

exptuple:
  exp COMMA exp { [$1; $3] }
| exp COMMA exptuple { $1 :: $3 }
;

explist:
  { Data("Nil", None) }
| exp { Data("Cons", Some (Tuple [$1 ; Data("Nil", None)])) }
| exp SEMICOLON explist { Data("Cons", Some (Tuple [$1 ; $3])) }
;

fn_patterns:
    pattern FN_ARROW exp { print "fp -> p -> e"; [($1,$3)] }
  | pattern FN_ARROW exp fn_patternsAux { print "fp -> e fpA"; ($1,$3) :: $4 }
;

fn_patternsAux:
    PIPE pattern FN_ARROW exp { print "fpA -> | p -> e"; [($2,$4)] }
  | PIPE pattern FN_ARROW exp fn_patternsAux { print "fpA -> | p -> e fpA"; ($2,$4) :: $5 }
;

pattern:
    const { print "p -> c";
				match $1 with
					 IntConst(i)  -> IntPat(i)
				  | BoolConst(b) -> BoolPat(b)
				  | Data(c, None) -> DataPat(c, None)
				  | _ -> raise ConstTypeParseError }
    | MINUS INT  { print ("p -> -int "^(string_of_int ($2 * -1))); IntPat($2 * -1) }
    | CONS pattern { DataPat($1, Some $2) }
    | WILDCARD { print "p -> _"; WildcardPat }
    | VAR { print ("p -> var "^$1^":t"); VarPat($1) }
    | LBRACK patlist RBRACK { $2 }
    | LPAREN RPAREN { TuplePat [] }
    | LPAREN pattuple RPAREN { TuplePat $2 }
    | LPAREN pattern RPAREN { print "p -> (p)"; $2 }
;

patlist:
   { DataPat("Nil", None) }
| pattern { DataPat("Cons", Some (TuplePat [$1 ; DataPat("Nil", None)])) }
| pattern SEMICOLON patlist { DataPat("Cons", Some (TuplePat [$1 ; $3])) }
;

pattuple:
  pattern COMMA pattern { [$1; $3] }
| pattern COMMA pattuple { $1 :: $3 }
;

