type tree =
  | E 
  | N of tree * char * tree

let equal t1 t2 =
  match (t1, t2) with
  | (E ,E ) -> true
  | (N (_,l1,c1,r1),N (_,l2,c2,r2)) ->
      (l1 == l2) && ((c1 == c2) && (r1 == r2))
  | _ -> false 

let leaf_x = N (E, 'x', E)
  
let rec create_tree n =
  if n = 0 then leaf_x, leaf_x
  else
    let n' = Random.int n in
    (* Format.eprintf "n: %d; n':%d@." n n'; *)
    let l1, l2 = create_tree n' in
    let r1, r2 = create_tree (n - n' - 1) in
    N (l1, 'm', r1), N (l2, 'm', r2)

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
  let b, t = Time.utime equal t1 t2 in
  eprintf "t1 =  t2? %b (%f s)@." b t