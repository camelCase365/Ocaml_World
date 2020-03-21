(*
   Sometimes we want to calculate a cumulative property over all
items in a data structure e.g find the sum of all the numbers,
find the largest number, or find the proportion of even numbers versus odd numbers.
These kinds of operations involve a traversal (or recursion) over
a data structure and the application of a function
*)

(* this function is also known as fold left 
   ('a -> 'b -> 'a) -> 'b list -> 'a -> 'a = <fun>
*)
let rec acc f l u =
match l with
| [] -> u
| h::t -> acc f t (f u h)

(* this function is also known as right fold.
  ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = <fun>
*)
let rec reduce f l u =
match l with
| [] -> u
| h::t -> f h (reduce f t u)


(* filter is also an iterator *)
let rec filter f l =
 match l with
 | [] -> []
 | h::t -> if f h
 then h::(filter f t)
 else filter f t