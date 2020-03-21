(*function that filtered out from a list all elements equal to a particular value. *)
let rec drop_val l v =
     match l with
     | [] -> []
     | hd::tl -> let nt = drop_val tl v in
                  if hd = v then nt else hd::nt