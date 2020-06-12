let curry f x y = f(x,y)

let uncurry f x y = f x y

let id = fun x -> x

let o f g = fun x -> f(g(x))

(* bounded iterations *)
                   
let rec iter n f =
  match n with
  | 0 -> id
  | _ -> o f (iter (n-1) f)

(* iter 4 (fun x -> x*x) 2;; *)

let fst = fun (x,y) -> x

let snd = fun (x,y) -> y;;

let fib n = fst(iter n (fun(x,y)->(x+y,x))(1,0))

(* unbounded iterations *)
              
let rec loop p f x =
  match (p x) with
  | true -> x
  | false -> loop p f (f x)

(* summations *)
               
let rec sigma f a b =
  if a > b then 0
  else (f a) + sigma f (a+1) b

let rec fact n =
  match n with
  | 0 -> 1
  | _ -> n * fact(n-1)


(* data structures *)
               
(* recursive types *)

type inttree = Leaf of int | Node of inttree * inttree

(* an object of type inttree is either a leaf with an integer
attached to it, or a binary node with two children of type inttree *)

Node(Leaf 3,Node(Leaf 4,Leaf 5))

let rec total = fun x ->
  match x with
  | Leaf n -> n
  | (Node(t1,t2)) -> total(t1)+total(t2);;

total (Node(Leaf 3,Node(Leaf 4,Leaf 5)));;

type exp = Constant of int
         | Variable of string
         | Addition of exp * exp
         | Multiplication of exp * exp;;

type dict_entry == {content:; key:};;

type intlist = Nil | Cons of int * intlist;;

let rec length list =
	  match list with
	    Nil -> 0
	  | Cons(_,tl) -> 1 + length tl
;;

length (Cons(1,Cons(2,Nil)));;

type 'a mylist = Nil | Cons of 'a * 'a mylist;;

let rec length list =
	match list with
	  Nil -> 0
	| Cons(_,tl) -> 1 + length tl
;;

length (Cons(1,Cons(2,Nil)));;

length (Cons("foo",Cons("bar",Nil)));;

type 'a bin_tree = 
     Empty
   | Node of 'a bin_tree * 'a * 'a bin_tree ;;

3::[];;

[3;2]@[1]

let rec length = fun l ->
  match l with
  |    [] -> 0
  | (a::l) -> 1 + length l;;


let rec append = fun l1 l2 ->
  match l1 with
  | [] -> l2
  | h1 :: t1 -> h1 :: append t1 l2;;

let rec rev = fun l ->
  match l with
  | [] ->  []
  | h :: t -> append (rev t) [h];;

let rec map = fun f l ->
  match l with
  | [] -> []
  | h :: t -> f(h) :: map f t;;

let rec reduce = fun f l acc ->
  match l with
  | [] -> acc
  | h :: t -> reduce f t (f acc h);;

let rec filter = fun f l ->
  match l with
  | [] -> []
  | h :: t -> if f h then
                h :: (filter f t)
              else
                filter f t;;

let add (x:int) (y:int) = x + y;;

let compose f g x = f (g x);;

let compose_fn_int (f : int -> int) (g : int -> int) (x:int) = compose f g x;;

(* representing trees *)

(* Binary trees *)

type 'a bin_tree =
  Empty
| Node of 'a bin_tree * 'a * 'a bin_tree;;

(* use this structure to define a little sorting program using binary search trees
-- a binary search tree has the property that all values in the left branch are less
than that of the root, and all those of the right branch are greater *)

(* a sorted list is extracted from a binary search tree via inorder traversal *)

let rec list_of_tree = function
    Empty -> []
  |  Node(lb, r, rb) -> (list_of_tree lb) @ (r :: (list_of_tree rb));;

(* insert: helper function to obtain a binary search tree from a list *)

let rec insert x = function
    Empty -> Node(Empty, x, Empty)
  | Node(lb, r, rb) -> if x < r then Node(insert x lb, r, rb)
                       else Node(lb, r, insert x rb);;

let rec tree_of_list = function
    [] -> Empty
  | h :: t -> insert h (tree_of_list t);;

(* sort function *)
let sort x = list_of_tree (tree_of_list x);;

let test_list = [5; 8; 2; 7; 1; 0; 3; 6; 9; 4];;



                                 
