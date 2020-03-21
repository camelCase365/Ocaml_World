type ty = BoolTy | IntTy | FunTy of ty * ty

type expr =
       Id of string                     (* for identifiers *)
     | Int of int                       (* for integers *)
     | True                             (* for the boolean value true *)
     | False                            (* for the boolean value false *)
     | Plus of expr * expr              (* for exp1 + exp2 *)
     | Minus of expr * expr             (* for exp1 - exp2 *)
     | Times of expr * expr             (* for exp1 * exp2 *)
     | Div of expr * expr               (* for exp1 / exp2, division being for integers *)
     | Lss of expr * expr               (* for exp1 < exp2 *)
     | Eq of expr * expr                (* for exp1 = exp2, = being equality comparison *)
     | Gtr of expr * expr               (* for exp1 > exp2 *)
     | And of expr * expr               (* for exp1 && exp2 *)
     | Or of expr * expr                (* for exp1 || exp2 *)
     | Not of expr                      (* for not exp *)
     | Cond of expr * expr * expr       (* for if exp1 then exp2 else exp3 *)
     | Let of string * expr * expr      (* for let <id> = exp1 in exp2 *)
     | Fun of string * ty * expr        (* for fun (x:ty) -> exp *)
     | App of expr * expr               (* for (exp1 exp2) *)



let exp1 = Let ("x",(Int 5),Let ("y",(Int 7),Plus ((Id "x"),(Id "y"))))
         

let exp2 = Fun("x",IntTy,Fun("y",BoolTy,Cond(Id "y",Plus(Id "x", (Int 1)),Minus(Id "x",Int 5))))


let exp3 = Times(Fun("x",IntTy,Times(Fun("y",IntTy,Plus(Id "x", Id "y")),Id "x")),Int (5))

(* Solution to Part 2 *)
             (*prediction: an expresion and list should be input to type checked
               the input and body of the function
                 *)
 let rec typeof_aux exp l =
    match exp with
    | Id x -> let ty = List.assoc x l in Some ty
    | True | False -> Some BoolTy
    |Plus(m,n) | Minus(m,n) -> (match (typeof_aux m l, typeof_aux n l) with
                                | (Some IntTy, Some IntTy) -> Some IntTy
                                | _ -> None)
    | Times(m,n) | Div(m,n) ->(match (typeof_aux m l, typeof_aux n l) with
                                | (Some IntTy, Some IntTy) -> Some IntTy
                                | _ -> None)
    | Lss(m,n) | Eq(m,n) | Gtr(m,n) -> (match (typeof_aux m l, typeof_aux n l) with
                                | (Some IntTy, Some IntTy) -> Some IntTy
                                | _ -> None)
    | And(m,n) | Or(m,n) -> (match (typeof_aux m l, typeof_aux n l) with
                                | (Some IntTy, Some IntTy) -> Some IntTy
                                | _ -> None)
    | Not m -> (match (typeof_aux m l) with
                | Some BoolTy
                | _ -> None)
    | Cond(m,n, h) -> (match typeof_aux m l with
                       | Some BoolTy -> (match(typeof_aux m l, typeof_aux h l)  with
                                         |(Some t1, Some t2) when t1 = t2 -> Some t1
                                         |_ -> None)
                       | _ -> None)
    | Let(str, n,h) -> (match typeof_aux n l with
                        | Some ty -> (match typeof_aux h ((str, ty)::l) with
                                                         | Some ty -> Some ty
                                                         |_ -> None)
                        | _ -> None)
    | Fun (str, ty,h) -> (match typeof_aux h ((str, ty)::l) with
                        | Some ty' -> Some (FunTy (ty',ty))
                        | _ -> None)
                                                

    | App(m,n) -> (match typeof_aux m l with
                   | Some FunTy(t1, t2) -> (match(typeof_aux n l) with
                                                  | Some ty -> if (t1 = t2) then Some ty else None
                                                  |_ -> None)
                   | _ -> None)


 let typeof exp = match typeof_aux exp [] with
   | Some ty -> Some ty
   |_ -> None