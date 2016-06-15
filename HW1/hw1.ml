let rec (member: 'a -> 'a list -> bool) =
    fun x s ->
        match s with
        | [] -> false
        | hd::tl -> 
        let t' = member x tl in
            if hd = x then true else t';;

let rec (add: 'a -> 'a list -> 'a list) =
    fun x s ->
    	match s with
    	| [] -> x::[]
    	| hd::tl ->
    	let t' = add x tl in
            if member x s then s else hd::t';;

let rec (union : 'a list -> 'a list -> 'a list) =
    fun s1 s2 ->
        match s1 with
        | [] -> s2
        | hd::tl ->
        let t' = union tl in
            if member hd s2 then t' s2 else (t' (add hd s2));;

let rec (fastUnion : 'a list -> 'a list -> 'a list) = 
    fun s1 s2 ->
        match s1 with
        | [] -> s2
        | hd::tl ->
            match s2 with
            | [] -> s1
            | hd2::tl2 -> 
            let t' = fastUnion in
                if hd < hd2 then hd::(t' tl s2)
                else (if hd2 < hd then hd2::(t' tl2 s1)
                else hd2::(t' tl tl2));;

let rec (intersection : 'a list -> 'a list -> 'a list) =
    fun s1 s2 ->
        List.filter(fun x -> member x s2) s1;;

let rec (setify : 'a list -> 'a list) =
    fun l ->
        match l with
        | [] -> []
        | hd::tl ->
        let t' = setify tl in
            if member hd tl then t' else hd::t';;

let rec (powerset : 'a list -> 'a list list) =
    fun s ->
        match s with
        | [] ->[[]]
        | hd::tl ->
            powerset(setify tl) @ (List.map(fun pw_l -> add hd (setify pw_l))(powerset (setify(tl))));;

let rec (partition : ('a -> bool) -> 'a list -> 'a list * 'a list) =
    fun f l -> 
   match l with
   | [] -> ([],[])
   | hd::tl -> 
   let (sati, not_sati) = partition f tl in
      ((if f hd then hd::sati else sati), (if not(f hd) then hd::not_sati else not_sati));;

let rec (whle : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a) = 
    fun p f x ->
        if not(p (f x)) then (f x) else whle p f (f x);;

(* Consulted ocaml.org/learn/taste.html for composition of functions *)
let rec (pow : int -> ('a -> 'a) -> ('a -> 'a)) =
    fun n f ->
        if n = 0 then (fun x -> x ) else
            (fun x -> f ( (pow (n-1) f) x));;
