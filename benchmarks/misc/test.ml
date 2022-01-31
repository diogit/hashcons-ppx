type%hashcons tree =
  | E 
  | N of tree * char * tree

let empty = E 
let node =
  let cpt = ref 1  in
  fun l  ->
    fun c  ->
      fun r  ->
        let n0 = N ((!cpt), l, c, r)  in
        let n = W.merge nodes n0  in if n == n0 then incr cpt; n
        
let leaf_x =
  node E 'x' E
  
let rec create_tree n =
  if n = 0 then leaf_x, leaf_x
  else
    let n' = Random.int n in
    (* Format.eprintf "n: %d; n':%d@." n n'; *)
    let l1, l2 = create_tree n' in
    let r1, r2 = create_tree (n - n' - 1) in
    node l1 'm' r1, node l2 'm' r2

open Format
module Time = struct

  open Unix

  let utime f x y =
    let u = (times()).tms_utime in
    let z = f x y in
    let ut = (times()).tms_utime -. u in
    (z,ut)

  let print f x y =
    let (y,ut) = utime f x y in
    printf "user time: %2.2f@." ut;
    y

end

let () =
  let t1, t2 = create_tree 100_000_000 in
  (* Format.eprintf "@[%a@]@." Tree.pp_tree t1; *)
  Gc.print_stat stderr;
  let b, t = Time.utime (==) t1 t2 in
  eprintf "t1 == t2? %b (%f s)@." b t;
  let b, t = Time.utime X.equal t1 t2 in
  eprintf "t1 =  t2? %b (%f s)@." b t