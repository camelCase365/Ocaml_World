(* a tree data structure is a union type by defining a disjoint union for the
type of a node and its children*)
type 'a tree =
| Leaf
| Node of 'a * 'a tree * 'a tree

(* build a simple data structure for
representing sets of values of type ’a .*)
(* The empty set is just a Leaf . To add an element to a set s , we create a new Node
with a Leaf as a left-child, and s as the right child.*)

(* One problem with the unbalanced tree defined here is that the complexity of the mem-
bership operation is O(n), where n is cardinality of the set.
We can can begin to address the performance by ordering the nodes in the tree.
The invariant we would like to maintain is the following: for any interior node
Node (x, left, right) , all the labels in the left child are smaller than x , and all the
labels in the right child are larger than x*)

let rec insert x = function
| Leaf -> Node(x, Leaf, Leaf)
| Node(y, left, right) as node ->
   if x < y then 
     Node(y, insert x left, right)
   else if x > y then
     Node(y, left, insert x right)
   else node
   

let rec set_of_lst s =
match s with
| [] -> Leaf
| h::t -> insert h (set_of_lst t)

let s = set_of_lst [3; 5; 7; 11; 13]


(* The membership function is defined recursively: an element x is a member of a tree iff
the tree is a Node and x is the label, or x is in the left or right subtrees. *)
let rec member e  l =
  match l with
  | Leaf -> false
  | Node(x, left, right) -> e = x || (e < x && member e left) || (e > y && member x right)