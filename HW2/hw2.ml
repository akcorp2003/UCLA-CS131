type vector = float list;;
type matrix = vector list;;

let (vplus : vector -> vector -> vector) =  
    fun v1 v2 ->
        List.map2 (fun f1 f2 -> f1 +. f2) v1 v2;;

let (mplus : matrix -> matrix -> matrix) =
    fun m1 m2 ->
       List.map2 (vplus) m1 m2;;

let (vmult : vector -> vector -> vector) =
    fun v1 v2 ->
        List.map2 (fun f1 f2 -> f1 *. f2) v1 v2;;

let (dotprod : vector -> vector -> float) =
    fun v1 v2 ->
        List.fold_left (fun cur_val val_vec -> cur_val +. val_vec) 0.0 (vmult v1 v2);;


        

(* "Expands" a vector v1 to become a matrix with List.length mat rows *)
let (createmat_fromvec : 'a list -> 'b list list -> 'a list list) =
    fun v1 mat ->
        List.map (fun matrow -> v1) mat;;

(* "Expands" a vector v1 to become a matrix with List.length v1 rows*)
let (createemptymat_fromvec : 'a list -> 'a list list) =
    fun v1 ->
        List.map (fun vec_elem -> []) v1;;

(* "Projects" the vector v1 into a matrix with the same number of rows as matrix m and multiplies the 
projected matrix and matrix m using the dotprod function written earlier. *)
let (vec_mult_mat : vector -> matrix -> vector) =
    fun v1 m ->
        if (List.length v1) = 0 then
            []
        else
            let createdmat = createmat_fromvec v1 m in
            List.map2 (dotprod) (createdmat) (m);;


let (transpose : matrix -> matrix) =
    fun m1 ->
        let emptymat = createemptymat_fromvec ((List.hd) m1) in
        let indexvec = List.mapi (fun index elem -> index) (List.hd m1) in
        let indexmat = createmat_fromvec indexvec m1 in
        let combined = List.map2 (List.combine) indexmat m1 in
        let flattenedcombinedm1 = List.flatten combined in
        let tupledtransposedmat = List.mapi (fun row_index emptyrow -> List.filter (fun elem ->
            match elem with
            | (index, value) -> if index = row_index then true else false) flattenedcombinedm1) emptymat in
        List.map (fun matrow ->
                    List.map (fun elem ->
                                match elem with
                                | ( _, value) -> value) matrow) tupledtransposedmat;; 

(* The main basis of this algorithm is to transpose matrix m2 so that its values are easily reachable 
by simple iteration. Then, we take each row of matrix m1, "expand" it into a matrix with the same number
of rows as matrix m2, and multiply using the dotprod function written earlier to make one row of the resulting
matrix. *)
let (mmult : matrix -> matrix -> matrix) =
    fun m1 m2 ->
        let t_mat2 = transpose m2 in
        List.map (fun m1_row -> vec_mult_mat m1_row t_mat2) m1;;

(* Problem 2 *)

type op = Plus | Minus | Times | Divide;;
type exp = Num of float | BinOp of exp * op * exp;;

(* Given an expression, the function will evalutate it and return a float. *)
let rec (evalExp : exp -> float) =
    fun exps ->
        match exps with
        | Num(i) -> i
        | BinOp(Num(i), oper, Num(j)) -> (match oper with
                                            | Plus -> evalExp(Num(i +. j))
                                            | Minus -> evalExp(Num(i -. j))
                                            | Times -> evalExp(Num(i *. j))
                                            | Divide -> evalExp(Num(i /. j))
                                         )
        | BinOp(l_value, oper, r_value) -> (match oper with
                                            | Plus -> evalExp(Num(evalExp(l_value) +. evalExp(r_value)))
                                            | Minus -> evalExp(Num(evalExp(l_value) -. evalExp(r_value)))
                                            | Times -> evalExp(Num(evalExp(l_value) *. evalExp(r_value)))
                                            | Divide -> evalExp(Num(evalExp(l_value) /. evalExp(r_value)))
                                            );;

type instr = Push of float | Swap | Calculate of op;;

(* A calculator helper function. Given a list of floats and an operation, the function will take the last 2
elements of the list and perform the operation on them. By taking the last 2 elements, the list is simulating
a stack. *)
let rec (cal_helper : float list -> op -> float list) =
    fun mylist oper ->
        if List.length mylist = 2 then
            (match oper with
            | Plus -> [List.nth (List.rev mylist) 1 +. List.hd (List.rev mylist)]
            | Minus -> [ List.nth (List.rev mylist) 1 -. List.hd (List.rev mylist)]
            | Times -> [List.nth (List.rev mylist) 1 *.  List.hd (List.rev mylist)]
            | Divide -> [List.nth (List.rev mylist) 1 /.  List.hd (List.rev mylist)]
            )
        else (List.hd mylist)::(cal_helper (List.tl mylist) oper);;

(* A generic swapper function. Takes the last 2 elements of a list and swaps their positions. *)
let rec (swap_helper : 'a list -> 'a list) =
    fun mylist ->
        if List.length mylist = 2 then
            List.rev mylist
        else (List.hd mylist)::swap_helper(List.tl mylist);;

(* Executes a list of instructions. 
Note: if the length of the instruction stack is greater than or equal to 2, then that means we still
have more operations to execute and so, we recursively call execute on the tail. The stack we pass to it will
be updated according to what instruction is currently being read. *)
let (execute : instr list -> float) =
    fun instructions ->
        let rec (mystack : float list -> instr list -> float list) =
            fun float_stack instr_stack ->
                match instr_stack with
                | hd::tl -> (match hd with
                            | Push(i) -> (if ((List.length (instr_stack)) >= 2) then mystack (float_stack@[i]) tl else [i])
                            | Calculate(operation) -> (if (List.length (instr_stack)) >= 2 then (mystack (cal_helper float_stack operation) tl) else cal_helper float_stack operation)
                            | Swap -> (if (List.length (instr_stack)) >= 2 then (mystack (swap_helper float_stack) tl) else swap_helper float_stack)
                            )
        in List.hd (List.rev (mystack ([]) instructions));;

(* Compiles an expression into instructions. Performs a post-order traversal to generate the instructions. *)
let rec (compile : exp -> instr list) =
    fun express ->
        match express with
        | Num(i) -> [Push(i)]
        | BinOp(l_value, operation, r_value) -> (match operation with
                                                | Plus -> (compile l_value)@(compile r_value)@[Calculate(Plus)]
                                                | Minus -> (compile l_value)@(compile r_value)@[Calculate(Minus)]
                                                | Times -> (compile l_value)@(compile r_value)@[Calculate(Times)]
                                                | Divide -> (compile l_value)@(compile r_value)@[Calculate(Divide)]
                                                );;

(* Removes the last two elements of a list. If the list's length is less than 2, then an empty list is returned. *)
let rec (remove_lasttwo : 'a list -> 'a list) =
    fun express_list ->
        if (List.length express_list <= 2) then [] 
        else (List.hd express_list)::(remove_lasttwo (List.tl express_list));;

(* Given a list of expressions, this function will merge the last two exp of the list into a BinOp() and return
the entire list with the new expression at the tail. *)
let rec (build_exp : exp list -> op -> exp list) =
    fun express_list oper ->
        match oper with
        | Plus -> (remove_lasttwo express_list)@[BinOp(List.nth (List.rev express_list) 1, Plus, List.hd (List.rev express_list))]
        | Minus -> (remove_lasttwo express_list)@[BinOp(List.nth (List.rev express_list) 1, Minus, List.hd (List.rev express_list))]
        | Times -> (remove_lasttwo express_list)@[BinOp(List.nth (List.rev express_list) 1, Times, List.hd (List.rev express_list))]
        | Divide -> (remove_lasttwo express_list)@[BinOp(List.nth (List.rev express_list) 1, Divide, List.hd (List.rev express_list))];;

(* Decompiles a set of instructions into an expression. The explanation for the ifs is the same as for execute. *)
let rec (decompile : instr list -> exp) =
    fun instructions ->
        let rec (mystack : exp list -> instr list -> exp list) =
            fun expressions instr_list ->
                match instr_list with
                | hd::tl -> ( match hd with
                             | Push(i) -> (if ((List.length instr_list) >= 2) then mystack (expressions@[Num(i)]) tl else [Num(i)])
                             | Calculate(operation) -> (if (List.length instr_list) >= 2 then (mystack (build_exp expressions operation) tl) else build_exp expressions operation)
                             | Swap -> (if (List.length instr_list) >=2 then (mystack (swap_helper expressions) tl) else swap_helper expressions)
                            )
            in List.hd (List.rev (mystack ([]) instructions));;
