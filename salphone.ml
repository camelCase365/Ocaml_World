
(* give a database create two function that find salary and phone of a person *)

let rec find_salary ls str =
  match ls with
  | [] -> None
  |(a,b,c)::t ->
     if (str = a) then Some c
     else (find_salary t str)

let rec find_phno ls str =
  match ls with
  | [] -> None
  |(a,b,c)::t ->
     if (str = a) then Some b
     else (find_phno t str)