type tree = E | N of int * tree * char * tree

let empty = E

let unique = function
    | E -> 0
    | N (u, _, _, _) -> u

module X = struct
    type t = tree
    let hash = function
        | E -> 0
        | N (_, l, c, r) -> (19 * (19 * unique l + Char.code c) + unique r) land max_int
    let equal t1 t2 =
        match t1, t2 with
        | E, E -> true
        | N (_, l1, c1, r1), N (_, l2, c2, r2) -> l1 == l2 && c1 == c2 && r1 == r2
        | _ -> false
end

module W = Weak.Make(X)
let nodes = W.create 5003

let node =
    let cpt = ref 1 in
    fun l c r ->
        let n0 = N (!cpt, l, c, r) in
        let n = W.merge nodes n0 in
        if n == n0
        then incr cpt;
        n

(* a = 97, z = 122 *)
let rec buildTree =
    let c = 97 in
    let rec buildTreeAux c =
        if  c = 122
        then N (0, N (0, E, (Char.chr (c mod 3)), E), (Char.chr c), E)
        else N (0, N (0, E, (Char.chr (c mod 3)), E), (Char.chr c), (buildTreeAux (c + 1)))
    in buildTreeAux c

let rec buildTreeHC =
    let c = 97 in
    let rec buildTreeAux c =
        if  c = 122
        then node (node E (Char.chr (c mod 3)) E) (Char.chr c) E
        else node (node E (Char.chr (c mod 3)) E) (Char.chr c) (buildTreeAux (c + 1))
    in buildTreeAux c

let oc = open_out_gen [Open_creat; Open_append] 0o666 "hc_results.txt" 

(* let tree = buildTree *)
let treeHC = buildTreeHC
let mem_used = Gc.allocated_bytes () 
let () = Printf.fprintf oc "Number of bytes allocated: %f MB\n" ((mem_used /. 1024.0) /. 1024.0)