type ('a, 'b) btree =
    Empty
  | Node of 'a * 'b * ('a, 'b) btree * ('a, 'b) btree


 (* Solution to Part 2 *)
let inittree = Empty

let tree1 = Node(14, "Abdikarim",Empty, Empty)
let tree2 = Node("Omar",("952-000-0000", 60000),Empty,Empty)

(*pre-conditon: the input has to have a key
invariant: every key has value or none *)
let rec find bst k =
  match bst with
  |Empty -> None
  |Node(key,value,l,r) ->
    if key = k then value else(if key > k then find r k else find l k)


 
let rec insert bst k d =
  match bst with
  |Empty -> Node(k, d,Empty,Empty)
  |Node (key,value,l,r) -> if key > k then insert r k d else insert l k d


let rec keylist bst =
  match bst with
  |Empty -> []
  |Node(k,v,l,r) -> (keylist r) @ (k::keylist l)

(* return the max key in a given node*)
let rec max_key bst =
  match bst with
  | Node(k,v,Empty,Empty) -> k
  | Node(k,v,l,r) -> max_key r (* maximum node is always on right side *)

(*return the max node *)

let rec max_val bst =
  match bst with
  | Node(k,v,Empty,Empty) -> v
  | Node(k,v,l,r) -> max_key

let rec delete bst key =
    match bst with
    | Empty -> Empty
    | Node (k,v,l,r) ->
       if key = k then aux l r
        else if k < key then Node(k,v,delete l key, r)
         else Node(k,v,l, delete r key)
  and aux l r =
    match(l,r) with
    |(l,Empty) -> l
    |(Empty,r) -> r
    | (_,_) -> let key2 = max_key r
               in Node (key2,max_val r,l,delete r key2)