let () =
  Random.self_init ()

module type T = sig

  type t

  val empty: t

  val node: t -> char -> t -> t

  val create_tree: int -> t * t

  val equal: t -> t -> bool

end

module HTree : T = struct

  type tree = E | N of int * tree * char * tree

  let unique = function
    | E -> 0
    | N (u, _, _, _) -> u

  module X = struct

    type t = tree

    let hash = function
      | E -> 0
      | N (_, l, c, r) ->
          (19 * (19 * unique l + Char.code c) + unique r) land max_int

    let equal t1 t2 = match t1, t2 with
      | E, E -> true
      | N (_, l1, c1, r1), N (_, l2, c2, r2) ->
          l1 == l2 && c1 == c2 && r1 == r2
      | _ -> false

  end

  module W = Weak.Make(X)
  let nodes = W.create 5003

  include X

  let empty = E

  let node =
    let cpt = ref 1 in
    fun l c r ->
      let n0 = N (!cpt, l, c, r) in
      let n = W.merge nodes n0 in
      if n == n0 then incr cpt;
      n

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

end

module Tree : T = struct

  type tree = E | N of tree * char * tree

  type t = tree

  let rec equal t1 t2 = match t1, t2 with
    | E, E -> true
    | N (l1, c1, r1), N (l2, c2, r2) ->
        equal l1 l2 && c1 = c2 && equal r1 r2
    | _ -> false

  let empty = E

  let node l c r =
    N (l, c, r)

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

  (* open Format *)

  (* let rec pp_tree fmt = function
   *   | E -> ()
   *   | N (l, c, r) ->
   *       fprintf fmt "@[<hov 2>%c@\n%a@\n%a@]" c pp_tree l pp_tree r *)

end

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

(* let () =
    let t1, t2 = Tree.create_tree 100_000_000 in
    (* Format.eprintf "@[%a@]@." Tree.pp_tree t1; *)
    Gc.print_stat stderr;
    let b, t = Time.utime (==) t1 t2 in
    eprintf "t1 == t2? %b (%f s)@." b t;
    let b, t = Time.utime Tree.equal t1 t2 in
    eprintf "t1 =  t2? %b (%f s)@." b t *)

let () =
  let t1, t2 = HTree.create_tree 100_000_000 in
  (* Format.eprintf "@[%a@]@." Tree.pp_tree t1; *)
  Gc.print_stat stderr;
  let b, t = Time.utime (==) t1 t2 in
  eprintf "t1 == t2? %b (%f s)@." b t;
  let b, t = Time.utime HTree.equal t1 t2 in
  eprintf "t1 =  t2? %b (%f s)@." b t
