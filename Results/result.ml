type term =
  | K 
  | Var of int * int 
  | Lam of int * term 
  | App of int * term * term 
let unique =
  function
  | K  -> 0
  | Var (u,_) -> u
  | Lam (id_lam,_) -> id_lam
  | App (id_app,_,_) -> id_app 
module X =
  struct
    type t = term
    let hash =
      function
      | K  -> 0
      | Var (u,_) -> 0
      | Lam (id_lam,_) -> 0
      | App (id_app,_,_) -> Hashtbl.hash id_app 
    let equal t1 t2 =
      match (t1, t2) with
      | (K ,K ) -> true
      | (Var (_,l_0),Var (_,r_0)) -> l_0 == r_0
      | (Lam (_,l_id_lam0),Lam (_,r_id_lam0)) -> l_id_lam0 == r_id_lam0
      | (App (_,l_id_app0,l_1),App (_,r_id_app0,r_1)) ->
          (l_id_app0 == r_id_app0) && (l_1 == r_1)
      | _ -> false 
  end
module W = (Weak.Make)(X)
let nodes = W.create 5003 
let empty = E 
let node =
  let cpt = ref 1  in
  fun l  ->
    fun c  ->
      fun r  ->
        let n0 = N ((!cpt), l, c, r)  in
        let n = W.merge nodes n0  in if n == n0 then incr cpt; n
  
