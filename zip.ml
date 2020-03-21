
(* create a function that zips files *)
let rec zip lp =
  match lp with
  | ([],_) -> []
  | (_,[]) -> []
  | ((x::l1),(y::l2)) -> (x,y) :: zip (l1,l2)