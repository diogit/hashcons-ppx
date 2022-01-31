type term =
  | Var of int * int 
  | Lam of int * term 
  | App of int * term * term 
let unique =
  function
  | Var (u,_) -> u
  | Lam (id_lam,_) -> id_lam
  | App (id_app,_,_) -> id_app 
module X =
  struct
    type t = term
    let hash =
      function
      | Var (_,u_0) -> 19 * (Hashtbl.hash u_0)
      | Lam (_,id_lam) -> 19 * (unique id_lam)
      | App (_,id_app,u_1) ->
          ((Hashtbl.hash id_app) + (19 * (unique u_1))) land max_int
      
    let equal t1 t2 =
      match (t1, t2) with
      | (Var (_,l_0),Var (_,r_0)) -> l_0 == r_0
      | (Lam (_,l_id_lam0),Lam (_,r_id_lam0)) -> l_id_lam0 == r_id_lam0
      | (App (_,l_id_app0,l_1),App (_,r_id_app0,r_1)) ->
          (l_id_app0 == r_id_app0) && (l_1 == r_1)
      | _ -> false 
  end
module W = (Weak.Make)(X)
let nodes = W.create 5003 
