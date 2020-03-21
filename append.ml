let rec append l m = 
match l with
| [] -> m
| (h::t) -> h::(append(t, m))