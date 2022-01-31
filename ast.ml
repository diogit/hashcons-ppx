type%hashcons term =
  | K
  | Var of int
  | Lam of [%hcons id_lam] * term
  | App of [%hcons id_app] * (term[@hash Hashtbl.hash]) * term

(* type term =
  | App of [%hcons id_app] * (int[@hash Hashtbl.hash])

let hash = function
  | App (_, x0) -> Hashtbl.hash x0 *)

type%hashcons term =
  | K
  | Var of [%hcons id_var] * int
  | Lam of term
  | App of term * term
  | Long of int * int * int * int * int