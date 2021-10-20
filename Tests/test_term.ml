type%hashcons term =
  | K
  | Var of int
  | Lam of [%hcons id_lam] * term
  | App of [%hcons id_app] * (term[@hash Hashtbl.hash]) * term