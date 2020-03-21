let rec reverse l1 l2  =
    match l1 with
    | [] -> l2
    | (h::t) -> reverse t (h::l2)