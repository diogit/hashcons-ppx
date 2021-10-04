type term =
    | Var of int
    | Lam of term
    | App of term * term

let x = Var 1
let y = Var 1

let table = Hashtbl.create 251
let hashcons x =
    try Hashtbl.find table x
    with Not_found -> Hashtbl.add table x x; x

(* false *)
let f = x == y 
let x = hashcons x
let y = hashcons y

(* true *)
let f = x == y

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
let %hashcons x = Var 1
let %hashcons y = Var 1

let x = hashcons (Var 1)
let y = hashcons (Var 1)