type variable = int
type bdd = 
  | Zero 
  | One
  | Node of int * variable * bdd * bdd

let unique = function
| Zero -> 0
| One -> 1
| Node (u, _,_,_) -> u 

module X =
  struct
    type t = bdd
    let hash =
      function
      | Zero  -> 0
      | One -> 1
      | Node (_, v,l,r) ->
          ((19 * ((19 * (unique l)) + (v))) + (unique r)) land
            max_int
      
    let equal bdd1 bdd2 =
      match (bdd1, bdd2) with
      | (Zero, Zero ) -> true
      | (One, One) -> true
      | (Node (_, v1,bddl1,bddr1), Node (_, v2,bddl2,bddr2)) ->
          (v1 == v2) && ((bddl1 == bddl2) && (bddr1 == bddr2))
      | _ -> false 
  end

module W = (Weak.Make)(X)
let nodes = W.create 5003 

let node =
  let cpt = ref 1  in
  fun v  ->
    fun l  ->
      fun r  ->
        let n0 = Node ((!cpt), v, l, r)  in
        let n = W.merge nodes n0  in if n == n0 then incr cpt; n

let mk v low high =
  if low == high then low
  else node v low high

let leaf_0 = node 0 Zero Zero
let leaf_1 = node 1 One One 
let rec create_tree n =
  if n = 0
  then (leaf_0, leaf_1)
  else
    (let n' = Random.int n  in
     let (l1,l2) = create_tree n'  in
     let (r1,r2) = create_tree ((n - n') - 1)  in
     ((node 0 l1 r1), (node 0 l2 r2)))

open Format
module Time =
  struct
    open Unix
    let utime f x y =
      let u = (times ()).tms_utime  in
      let z = f x y  in let ut = (times ()).tms_utime -. u  in (z, ut) 
    let print f x y =
      let (y,ut) = utime f x y  in printf "user time: %2.2f@." ut; y 
  end
let () =
  let (t1,t2) = create_tree 100_000_000  in
  Gc.print_stat stderr;
  (let (b,t) = Time.utime (==) t1 t2  in
   eprintf "t1 == t2? %b (%f s)@." b t;
   (let (b,t) = Time.utime X.equal t1 t2  in
    eprintf "t1 =  t2? %b (%f s)@." b t))