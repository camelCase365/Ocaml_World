(* adding together elements of a list *)
let rec sum l = 
     match l with
     | [] -> []
     | hd::tl -> hd + sum tl

(* for testing use sum [1;2;3;4] on ocaml compiler *)