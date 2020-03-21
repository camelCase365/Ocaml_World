
(*
    We want to modify every item in a data structure e.g list using a
function we have at hand
*)
(* 
  takes two arguments: a list and a function for transforming the elements
of that list. It returns a new list with the transformed elements and does not
modify the original list.
*)

let rec map f l = 
match l with
| [] -> []
| h::t -> (f h) :: map f t

(* map with tail recursive *)

(* first define an acculumator function. the function collect list in reverse order *)
let rec rev acc l = 
match l with
| [] -> acc
| h::t -> rev (h::acc) t

let rec rev_map f l acc = 
match l with
| [] -> acc
| h::t -> rev_map f (f h::acc) t

let map2 f l = rev [] (rev_map f [] l)