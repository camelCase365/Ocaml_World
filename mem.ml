let rec mem x l = 
match l with
 | [] -> false
 | h::t -> x = h || mem x t