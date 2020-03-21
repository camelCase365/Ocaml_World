(* val divide_list : ('a -> bool) -> 'a list -> 'a list * 'a list = <fun>
  precondition: the two inputs should be  a fun and a divide_list
  invariant: every input to divide_list outputs a list of tuple
*)
let rec eva bf l =
  match l with
  | [] -> []
  | (h::t) -> if bf h then h::eva bf t else eva bf t
let rec eva2  bf l =
  match l with
  | [] -> []
  | (h::t) -> if not (bf h) then h::eva2 bf t else eva2 bf t
 let divide_list bf l =
      (eva bf l,eva2 bf l)