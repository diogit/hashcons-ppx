type variable = int
type bdd = 
  | Zero 
  | One
  | Node of variable * bdd * bdd

let unique = function
| Zero -> 0
| One -> 1
| Node (u,_,_) -> u 

module X =
  struct
    type t = bdd
    let hash =
      function
      | Zero  -> 0
      | One -> 1
      | Node (v,l,r) ->
          ((19 * ((19 * (unique l)) + (v))) + (unique r)) land
            max_int
      
    let equal bdd1 bdd2 =
      match (bdd1, bdd2) with
      | (Zero, Zero ) -> true
      | (One, One) -> true
      | (Node (v1,bddl1,bddr1), Node (v2,bddl2,bddr2)) ->
          (v1 == v2) && ((bddl1 == bddl2) && (bddr1 == bddr2))
      | _ -> false 
  end

module W = (Weak.Make)(X)
let nodes = W.create 5003 