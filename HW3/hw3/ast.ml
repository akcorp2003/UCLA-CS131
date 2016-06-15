(* The syntax and representation of MOCaml patterns.

  p ::= intconst | boolconst | _ | var | (p1,...,pn) | C | C p
  intconst ::= integer constant
  boolconst ::= true | false
  var ::= variable -- an identifier whose first letter is lowercase
  C ::= data constructor -- an identifier whose first letter is uppercase

*)
type mopat =
    IntPat of int
  | BoolPat of bool
  | WildcardPat
  | VarPat of string
  | TuplePat of mopat list
  | DataPat of string * mopat option
;;

(* The syntax and representation of MOCaml expressions.

e ::= intconst | boolconst | var | e1 op e2 | -e | if e1 then e2 else e3
       | function p -> e | e1 e2
       | match e with p1 -> e1 '|' ... '|' pn -> en
       | (e1,...,en)
       | C | C e

op ::= + | - | * | = | > 

You should assume that the binary operators above apply only to integers;
raise a DynamicTypeError for other cases. 
   
*)
type moop = Plus | Minus | Times | Eq | Gt
type moexpr =
    IntConst of int
  | BoolConst of bool
  | Var of string
  | BinOp of moexpr * moop * moexpr
  | Negate of moexpr 
  | If of moexpr * moexpr * moexpr
  | Function of mopat * moexpr
  | FunctionCall of moexpr * moexpr
  | Match of moexpr * (mopat * moexpr) list
  | Tuple of moexpr list
  | Data of string * moexpr option
;;

(* The syntax and representation of Mocaml declarations.

d ::= e | let x = e | let rec f p = e

Note that for a "let rec" MOCaml uses only the syntax shown above,
which ensures that a function is declared.  In particular, in (M)OCaml
the syntax "let rec f p = e" is shorthand for
"let rec f = function p -> e".
   
*)
type modecl =
  | Expr of moexpr
  | Let of string * moexpr
  | LetRec of string * moexpr



(* The representation of MOCaml values, which are the results of
evaluating expressions.

  v ::= intconst | boolconst | function p -> e | (v1,...,vn) | C | C v
*)
type movalue =
    IntVal of int
  | BoolVal of bool
      (* A function value carries its lexical environment with it! *)
      (* If the function is recursive it also carries around its own name
         (the "string option" component below). *)
  | FunctionVal of string option * mopat * moexpr * moenv
  | TupleVal of movalue list
  | DataVal of string * movalue option

(* The representation of environments, which map strings to values.
See env.ml for the definition of the Env.env type and associated
operations. Env.env is polymorphic over the type for values, so here
we define moenv as a version of Env.env specialized to use the type
movalue for values. *)                                
and moenv = movalue Env.env

(* The result from evaluating a declaration -- an optional name that
was declared along with the value of the right-hand-side
expression. *)
type moresult = string option * movalue

                         
