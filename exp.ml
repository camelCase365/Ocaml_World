type expr =
       Id of string                   (* for identifiers *)
     | Int of int                     (* for integers *)
     | True                           (* for the boolean value true *)
     | False                          (* for the boolean value false *)
     | Plus of (expr * expr)          (* for exp1 + exp2 *)
     | Minus of (expr * expr)         (* for exp1 - exp2 *)
     | Times of (expr * expr)         (* for exp1 * exp2 *)
     | Div of (expr * expr)           (* for exp1 / exp2, division being for integers *)
     | Lss of (expr * expr)           (* for exp1 < exp2 *)
     | Eq of (expr * expr)            (* for exp1 = exp2, = being equality comparison *)
     | Gtr of (expr * expr)           (* for exp1 > exp2 *)
     | And of (expr * expr)           (* for exp1 && exp2 *)
     | Or of (expr * expr)            (* for exp1 || exp2 *)
     | Not of expr                    (* for not exp *)
     | Cond of (expr * expr * expr)   (* for if exp1 then exp2 else exp3 *)
     | Let of (string * expr * expr)  (* for let <id> = exp1 in exp2 *)
     | Fun of (string * expr)         (* for fun (x:ty) -> exp *)
     | App of (expr * expr)           (* for (exp1 exp2) *)


(*precondition: the input has to a string and an expression
  invariant: given an expression and a string freeIn will evaluate the inputs.
*)
let rec freeIn e s =
  match e with
  | Id x -> if x = s then true else false
  | Int x -> false
  | True -> false
  | False -> false
  | Plus(x,y) -> freeIn x s || freeIn y s
  | Minus(x,y) -> freeIn x s || freeIn y s
  | Times(x,y) -> freeIn x s || freeIn y s
  | Div(x,y) -> freeIn x s || freeIn y s
  | Lss(x,y) -> freeIn x s || freeIn y s
  | Eq(x,y) -> freeIn x s || freeIn y s
  | And(x,y) -> freeIn x s || freeIn y s
  | Gtr(x,y) -> freeIn x s || freeIn y s
  | Or(x,y) -> freeIn x s || freeIn y s
  | Not (x) -> freeIn x s
  | Cond(x,y,z) -> freeIn x s || freeIn	y s || freeIn z s
  | Let(x,ex,ey) -> if x = s then freeIn ex s else freeIn ex s || freeIn ey s
  | Fun(x,ex) -> if x = s then false else freeIn ex s
  | App(x,y)-> freeIn x s || freeIn y s

let namecounter = ref 0
let newname () =
     ( namecounter := !namecounter + 1; "var" ^ string_of_int !namecounter)


(*precondition: the input has to a string and two expressions
  invariant: given two expression and a string subst will substitute the string
  into the right right expression.
*)
let rec subst e n r =
  match e with
  | Id x -> if x = n then r else e
  | Int x -> Int x
  | True -> e
  | False -> e
  | Plus(x,y) -> Plus(subst x n r, subst y n r)
  | Minus(x,y) -> Minus(subst x n r, subst y n r)
  | Times(x,y) -> Times(subst x n r, subst y n r)
  | Div(x,y) -> Div(subst x n r, subst y n r)
  | Lss(x,y) -> Lss(subst x n r, subst y n r)
  | Eq(x,y) -> Eq(subst x n r, subst y n r)
  | Gtr(x,y) -> Gtr(subst x n r, subst y n r)
  | And(x,y) -> And(subst x n r, subst y n r)
  | Or(x,y) -> Or(subst x n r, subst y n r)
  | Not x -> Not(subst x n r)
  | Cond(x,y,z) -> Cond(subst x n r, subst y n r, subst z n r)
  | App(x,y) -> App(subst x n r, subst y n r)
  | Let(s, e1, e2) -> if s = n then Let(s, subst e1 n r, e2)
                      else (if freeIn r s = false then Let(s, subst e1 n r, subst e2 n r) else let x = newname() in
                                                                                                Let(x, subst e1 n r, subst(subst e2 s (Id x)) n r))
  | Fun(s, e) -> if s = n then Fun(s,e) else (if freeIn r s = false then Fun(s, subst e n r) else
                                                           let x = newname() in Fun(x, subst(subst e s (Id x)) n r))
