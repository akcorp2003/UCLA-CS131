(* Name: Aland Kuang

   UID: 104157973

   Others With Whom I Discussed Things:

   Other Resources I Consulted:
   
*)

(* EXCEPTIONS *)

(* This is a marker for places in the code that you have to fill in.
   Your completed assignment should never raise this exception. *)
exception ImplementMe of string

(* This exception is thrown when a type error occurs during evaluation
   (e.g., attempting to invoke something that's not a function).
   You should provide a useful error message.
*)
exception DynamicTypeError of string

(* This exception is thrown when pattern matching fails during evaluation. *)  
exception MatchFailure  

(* EVALUATION *)

(* See if a value matches a given pattern.  If there is a match, return
   an environment for any name bindings in the pattern.  If there is not
   a match, raise the MatchFailure exception.
*)
let rec patMatch (pat:mopat) (value:movalue) : moenv =
(
  match (pat, value) with
      (* an integer pattern matches an integer only when they are the same constant;
	 no variables are declared in the pattern so the returned environment is empty *)
    | (IntPat(i), IntVal(j)) when i=j -> Env.empty_env()
    | (BoolPat(b), BoolVal(bv)) when b=bv -> Env.empty_env()
    | (WildcardPat, _ ) -> Env.empty_env() (* No variables declared here *)
    | (VarPat(varname), _ ) -> Env.add_binding varname (value) (Env.empty_env())   
    | (TuplePat(tuplist), TupleVal(vals)) -> (try List.fold_left2 (fun tenv apattern aval -> 
                                                                Env.combine_envs (patMatch apattern aval) tenv
                        ) (Env.empty_env()) tuplist vals with Invalid_argument( _ ) -> raise (MatchFailure))
    | (DataPat(name, data_pat), DataVal(v_name, v_data_val)) -> ( match v_data_val with
                                                                 | None -> (match data_pat with
                                                                            | None -> Env.empty_env() (* Similar to the case of int to int*)
                                                                            | Some data_pat -> raise (MatchFailure))
                                                                 | Some v_data_val -> (match data_pat with
                                                                           | None -> raise(MatchFailure)
                                                                           | Some data_pat -> Env.combine_envs (patMatch data_pat v_data_val) (Env.empty_env()))
                                                                )
 
    | _ -> raise (MatchFailure)
)
    
(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)
let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
      (* an integer constant evaluates to itself *)
    | IntConst(i) -> IntVal(i)
    | BoolConst(b) -> BoolVal(b)
    | Negate(eo) -> let vo = evalExpr eo env in
                    (match vo with
                    | IntVal(i) -> IntVal(-i)
                    | _ -> raise (DynamicTypeError "You so stupid, you don't know that you can only negate integers."))
    | BinOp(expr_l, op, expr_r) -> ( match op with
                                    | Plus -> (let leftside = evalExpr expr_l env in
                                               let rightside = evalExpr expr_r env in
                                                match (leftside, rightside) with
                                                | (IntVal(i), IntVal(j)) -> IntVal(i + j)
                                                | _ -> raise (DynamicTypeError "You can only add two integers dumbo."))
                                    | Minus -> (let leftside = evalExpr expr_l env in
                                                let rightside = evalExpr expr_r env in
                                                match (leftside, rightside) with
                                                | (IntVal(i), IntVal(j)) -> IntVal(i - j)
                                                | _ -> raise (DynamicTypeError "You can only subtract two integers dumbo."))
                                    | Times -> (let leftside = evalExpr expr_l env in
                                                let rightside = evalExpr expr_r env in
                                                match (leftside, rightside) with
                                                | (IntVal(i), IntVal(j)) -> IntVal (i * j)
                                                | _ -> raise (DynamicTypeError "You can only multiply two integers dumbo."))
                                    | Eq -> (let leftside = evalExpr expr_l env in
                                             let rightside = evalExpr expr_r env in
                                             match (leftside, rightside) with
                                             | (IntVal(a), IntVal(b)) -> BoolVal(a = b)
                                             | _ -> raise (DynamicTypeError "You can only compare int to int, so stupid..."))
                                    | Gt -> (let leftside = evalExpr expr_l env in
                                             let rightside = evalExpr expr_r env in
                                             match (leftside, rightside) with
                                            | (IntVal(i), IntVal(j)) -> BoolVal( i > j)
                                            | _ -> raise (DynamicTypeError "You can only compare int to int...you need to go back to kindergarten!"))
    )
    | Var(name) -> (let molookval = (try (Env.lookup name env) with  Env.NotBound -> raise (DynamicTypeError "Couldn't find the variable. Please declare it first and then go get an IQ test.")) in
                    match molookval with
                    | IntVal(i) -> IntVal(i)
                    | BoolVal(i) -> BoolVal(i)
                    | TupleVal(lst) -> TupleVal(lst)
                    | FunctionVal(name, pat, expr, f_env) -> ( match name with
                                                    | None -> FunctionVal( None, pat, expr, f_env)
                                                    | Some name -> FunctionVal(Some(name), pat, expr, f_env)
                                                    )
                    | DataVal(name, data_expr) -> (match data_expr with
                                                    | None -> DataVal(name, None)
                                                    | Some data_expr -> DataVal(name, Some(data_expr))
                                                    )
                    )
    | If(cond, exec1, execelse) -> (let condresult = evalExpr cond env in
                                    match condresult with
                                    | BoolVal(res) -> (if res then evalExpr exec1 env else evalExpr execelse env)
                                    | _ -> raise (DynamicTypeError "Something went wrong with your if statement. Check to makes sure it's alright.")
                                    )
    
    | Match(expr, lst_pat_expr) -> let v = evalExpr expr env in 
                                    let rec match_helper pat_lst = 
                                    match pat_lst with
                                  | [] -> raise (MatchFailure)
                                  | (pat, expr)::rest -> try evalExpr expr (Env.combine_envs (patMatch pat v) env) with MatchFailure -> match_helper rest
                                    in match_helper lst_pat_expr

    | Tuple(val_list) -> TupleVal(List.map(fun expr -> evalExpr expr env) val_list)
    
    | Function(arg_pat, body_expr) -> FunctionVal(None, arg_pat, body_expr, env)

    | Data(name, data_expr) -> ( match data_expr with
                                | None -> DataVal(name, None)
                                | Some data_expr -> DataVal(name, Some (evalExpr data_expr env))

                    )


    | FunctionCall (funct, args) -> match funct with
                                    | Function(para_pat, body_expr) -> evalExpr body_expr (Env.combine_envs (patMatch para_pat (evalExpr args env)) env)
                                    | Var(name) -> (let functval = (try (Env.lookup name env) with Env.NotBound -> raise (DynamicTypeError "The function name doesn't exist. You need to declare it first and then go get an IQ test.")) in
                                                (match functval with
                                                | FunctionVal(name, para_pat, body_expr, funct_env) -> ( match name with
                                                                        | None -> evalExpr (body_expr) (Env.combine_envs (patMatch para_pat (evalExpr args env)) funct_env)
                                                                        | Some funct_name -> evalExpr (body_expr) (Env.combine_envs (Env.add_binding (funct_name) (FunctionVal(Some funct_name, para_pat, body_expr, funct_env)) (funct_env)) (patMatch para_pat (evalExpr args env)))


(* ( Env.combine_envs env (Env.add_binding (funct_name) (FunctionVal(Some funct_name, para_pat, body_expr, env)) (env)) ) *)
                                                                        )
                                                | _ -> raise (DynamicTypeError "What I found was not a function. Now I shall go slap myself.")
                                                ))                       
                                    | _ -> match evalExpr funct env with
                                            | FunctionVal(None, arg_pat, body_expr, funct_env) -> evalExpr body_expr (Env.combine_envs (patMatch arg_pat (evalExpr args env)) funct_env)
 
   | _ -> raise (ImplementMe "expression evaluation not implemented") 


(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's expression.
*)
let rec evalDecl (d:modecl) (env:moenv) : moresult =

  match d with
      (* a top-level expression has no name and is evaluated to a value *)
    | Expr(e) -> (None, evalExpr e env)
    | Let(s, e) -> (Some s, evalExpr e env)
    | LetRec(s, e) -> (match e with
                      | Function(pat, expr) -> (Some s, FunctionVal(Some s, pat, expr, env))
                      | FunctionCall(funct_bod, args) -> raise (DynamicTypeError "Whoa!")
                    )
    | _ -> raise (ImplementMe "let rec not implemented")

