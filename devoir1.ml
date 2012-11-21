(* Clément Lassieur *)

let hello_world () = print_string("Hello World!");;

let add () =
  let a = read_int() in
  let b = read_int() in
  print_int(a+b);;

(*
  add();;
*)

let rec fact n = match n with
  | 0 -> 1
  | n -> n * fact(n-1)
;;

fact 4;;


let rec fib n = match n with
  | 0 -> 0
  | 1 -> 1
  | n -> fib(n-1) + fib(n-2)
;;

fib 8;;


let rec f l =  match l with
  | [] -> print_string "I ’ m empty "
  | [_] -> print_string "I have one element "
  | _ :: _ -> print_string " I have more than one element "
;;

let rec rev l = match l with
  | [] -> []
  | x::l -> (rev l)@[x]
;;

rev([1;2;3]);;


let rec map f l = match l with
  | [] -> []
  | x::l -> [f x]@(map f l)
;;

let plus_deux x = x + 2;;

map plus_deux [1;2;3];;

let rec iter f l = match l with
  | [] -> ()
  | x::l -> f x; iter f l
;;

let print_plus_deux x = print_int(x + 2);;

iter print_plus_deux [1;2;3];;

(* reduce *)
let rec fold f a l = match l with
  | [] -> a
  | b::l -> f (fold f a l) b
;;

let plus a b = a + b;;

fold plus 0 [1;2;3];;

let square a = a * a;;

let square_list l = map square l;;

let list = square_list [3;2;1];;

let sum_list l = fold plus 0 l;;

sum_list [1;9;3];;

let set_mem a l = match l with
  | [] -> false
  | b::l when b = a -> true
  | b::l -> set_mem a l
;;

set_mem 9 list;;
set_mem 8 list;;

let rec set_add a l = match l with
  | [] -> [a]
  | b::l when a = b -> b::l
  | b::l -> [b]@(set_add a l)
;;

let test = set_add 23 list;;
let test2 = set_add 4 list;;

let rec set_union l1 l2 = match l1 with
  | [] -> l2
  | b::l1 -> set_union l1 (set_add b l2)
;;

set_union [1;2;4;7] [89;7;5;12;17];;

let rec set_intersection l1 l2 = match l1 with
  | [] -> []
  | b::l1 when set_mem b l2 -> set_add b (set_intersection l1 l2)
  | b::l1 -> set_intersection l1 l2
;;

set_intersection [1;2;4;7;12] [89;7;5;12;17];;

type 'a tlist =
 | Nil
 | Cons of 'a * 'a tlist
;;

Nil;;

Cons(Nil,Cons(Nil, Nil));;

let rec tmap f l = match l with
  | Nil -> Nil
  | Cons(x,l) -> Cons(f x,tmap f l)
;;

tmap plus_deux (Cons(1,Cons(2,Cons(3,Nil))));;

let rec titer f l = match l with
  | Nil -> ()
  | Cons(x,l) -> f x; titer f l
;;

titer print_plus_deux (Cons(1,Cons(2,Cons(3,Nil))));;

(* reduce *)
let rec tfold f a l = match l with
  | Nil -> a
  | Cons(b,l) -> f (tfold f a l) b
;;

tfold plus 0 (Cons(1,Cons(2,Cons(3,Nil))));;

let rec tlength l = match l with
  | Nil -> 0
  | Cons(a,l) -> 1 + tlength l
;;

tlength (Cons(1,Cons(2,Cons(3,Nil))));;

let rec to_list l = match l with
  | Nil -> []
  | Cons(a,l) -> [a]@(to_list l)
;;

to_list (Cons(1,Cons(2,Cons(3,Nil))));;
