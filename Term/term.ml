type term =
  | Var of int * int 
  | Lam of int * term 
  | App of int * term * term 

let unique = function
    | Var (u,_) -> u
    | Lam (u,_) -> u
    | App (u,_,_) -> u 

module X =
  struct
    type t = term
    let hash =
      function
      | Var (_,u_0) -> 19 * (Hashtbl.hash u_0)
      | Lam (_,u_0) -> 19 * (unique u_0)
      | App (_,u_0,u_1) ->
          ((19 * (Hashtbl.hash u_0)) + (19 * (unique u_1))) land max_int
      
    let equal t1 t2 =
      match (t1, t2) with
      | (Var (_,l_0),Var (_,r_0)) -> l_0 == r_0
      | (Lam (_,l_0),Lam (_,r_0)) -> l_0 == r_0
      | (App (_,l_0,l_1),App (_,r_0,r_1)) -> (l_0 == r_0) && (l_1 == r_1)
      | _ -> false 
  end
module W = (Weak.Make)(X)
let nodes = W.create 5003 

let var v =
    let ctr = ref 1 in
        let x0 = Var(!ctr, v) in
        let x = W.merge nodes x0 in
        if x == x0 then incr ctr;
        x

let lam t =
    let ctr = ref 1 in
    let x0 = Lam(!ctr, t) in
    let x = W.merge nodes x0 in
        if x == x0 then incr ctr;
        x

let app t1 t2 =
    let ctr = ref 1 in
    let x0 = App(!ctr, t1, t2) in
    let x = W.merge nodes x0 in
        if x == x0 then incr ctr;
        x

let rec lift n = 
    let rec lift_k k = function
    | Var(u, i) -> if i < k then Var(u, i) else Var(u, n + i)
    | Lam(u, t) -> Lam(u, lift_k (k + 1) t)
    | App(u, t1, t2) -> App (u, lift_k k t1, lift_k k t2)
    in lift_k 0

let rec subst w = 
    let rec subst_w n = function
    | Var(u, k) -> if k = n then lift n w
                   else if k < n then Var(u, k)
                   else Var(u, k - 1)
    | Lam(u, t) -> Lam (u, subst_w (n+1) t)
    | App(u, t1, t2) -> App (u, (subst_w n t1), (subst_w n t2))
in subst_w 0

let rec hnf = function
    | Var (u, n) -> Var (u, n)
    | Lam (u, t) -> Lam (u, hnf t)
    | App (u, t1, t2) ->
        match hnf t1 with
        | Lam (_, w) -> hnf (subst t2 w)
        | h -> App(u, h, t2)

let rec nf = function
    | Var(u, n) -> Var(u, n)
    | Lam(u, t) -> Lam(u, nf t)
    | App(u, t1, t2) -> 
        match hnf t1 with
        | Lam (_, w) -> nf (subst t2 w)
        | h -> App(u, nf h, nf t2)

let x = var 1
let delta = lam x
let omega = app delta delta

let () = assert (var 1 == x)
let () = assert (app x x == app x x)