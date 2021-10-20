type term =
     Ref of int
   | Abs of term
   | App of term * term

let rec lift n =
  lift_rec 0 
  and lift_rec k = lift_k
  and lift_k = fun
   Ref i -> if i < k
           then Ref i (* bound variables are invariant *)
           else Ref (n+i) (* free variables are relocated by n *)
  | Abs t -> Abs (lift_rec (k+1) t)
  | App t u -> App (lift_k t) (lift_k u)

let rec subst w =
  subst_w 0
  and subst_w n = fun
  Ref k -> if k = n
           then lift n w (* substituted variable *)
           else if k < n
           then Ref k (* bound variables *)
           else Ref (k-1) (* free variables *)
| Abs t -> Abs (subst_w (n+1) t)
| App t u -> App (subst_w n t) (subst_w n u)

let rec hnf = fun
  Ref n -> Ref n
  | Abs t -> Abs (hnf t)
  | App t u -> match hnf t with
    | Abs w -> hnf (subst u w)
    | h -> App h u

let rec nf = fun
  Ref n ->Ref n
  | Abs t -> Abs (nf t)
  | App t u -> match hnf t with
    Abs w -> nf (subst u w)
    | h -> App (nf h) (nf u) 