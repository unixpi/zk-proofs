let l = [4; 11; 8; 1];;
let m = [1; -1; 1; -1];;

let rec len = fun l ->
  match l with
  | [] -> 0
  | h :: t -> 1 + len t;;

let equal_len = fun l1 l2 -> len l1 == len l2;;

(* check all elements in m are either -1 or 1 *)

let rec check_list = fun l f ->
  match l with
  | [] -> true
  | h :: t -> if f h then check_list t f else false;;

let one_or_minus_one = fun e -> e == 1 || e == -1;;

let check_m = fun m -> check_list m one_or_minus_one;;
    
let rec sum = fun l ->
  match l with
  | [] -> 0
  | h :: t -> h + sum t;;
  
let rec list_mult = fun l1 l2 ->
  match l1, l2 with
  | [], [] -> []
  | h1 :: t1, h2 :: t2 -> (h1 * h2) :: list_mult t1 t2;;

let dot_product = fun l1 l2 -> sum(list_mult l1 l2);;

let check_dot_product_is_zero = fun l1 l2 -> dot_product l1 l2 == 0;;

(* partial sum list of l's dot product with m (remember to prefix with 0)*)
let rec partial_sum_dot_product = fun l1 l2 sum ->
  match l1, l2 with
  | [], [] -> []
  | h1 :: t1, h2 :: t2 -> (sum + h1 * h2) :: partial_sum_dot_product t1 t2 (sum + h1 * h2);;

(* manufacturing zero knowledge *)

(* 1. *)
let coin_flip = fun () -> Random.bool ();;

let side_obfuscator = 1 - 2 * (Random.int 1);;
                                               
let rec map = fun f l ->
  match l with
  | [] -> []
  | h :: t -> (f h) :: map f t;;

let flip_signs = fun l -> map (fun x -> -1 * x) l;;

(* 2. *)
let random_int = fun max -> Random.int max;;

let add_random_int = fun x max -> x + Random.int max;;

let rec max_int = fun l mx ->
  match l with
  | [] -> mx
  | h :: t -> if h > mx then
                max_int t h
              else
                max_int t mx;;
                
(* The first piece of code we'll need, something that takes a problem (i.e. l) and a satisfying assignment (i.e. m) and constructs a witness (i.e. p) that will attest to the satisfiability of the problem instance *)

(*  Given an instance of a partition problem via a list of numbers (the problem)
 and a list of (-1, 1), we say that the assignment satisfies the problem
 if their dot product is 0. *)

let get_witness = fun problem assignment ->
  if equal_len problem assignment &&
       check_m assignment &&
         check_dot_product_is_zero problem assignment then
    let rand_int = random_int (max_int problem 0) in
    if coin_flip() then
      let m = flip_signs assignment
            in map (fun x -> x + rand_int)(0 :: (partial_sum_dot_product problem m 0))
    else
      map (fun x -> x + rand_int)(0 :: (partial_sum_dot_product problem assignment 0))
  else
    [];;
        
  
  
  
