(* give an element drop it from a list *)
let rec drop n l =
    match l with
    | [] -> 0::[]
    | (h::t) -> if (n = 0) then l else drop (n-1) t