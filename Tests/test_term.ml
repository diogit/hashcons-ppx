(* type%hashcons term =
  | K
  | Var of int
  | Lam of [%hcons id_lam] * term
  | App of [%hcons id_app] * (term[@hash Hashtbl.hash]) * term *)

type%hashcons term =
  | Var of int
  | Lam of [%hcons id_lam] * term
  | App of [%hcons id_app] * (term[@hash Hashtbl.hash]) * term

(* type%hashcons term =
  | K
  | Var of [%hcons id_var] * int
  | Lam of term
  | App of term * term
  | Long of int * int * int * int * int *)