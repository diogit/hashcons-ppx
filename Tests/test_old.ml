type t =
  | E 
  | N of t * t

let %hashcons l = E

let %hashcons tree = N(l,l)
let %hashcons tree = N(tree, tree)

let rec create_tree h =
    if h > 0
    then N(create_tree (h - 1), create_tree (h - 1))
    else E

let tree1 = create_tree 10
let oc = open_out_gen [Open_creat; Open_append] 0o666 "Results/mem_results.txt"
let mem_used = Gc.allocated_bytes ()
let () = Printf.fprintf oc "Number of bytes allocated: %f MB\n" (mem_used/.1024.0/.1024.0);
  Gc.print_stat stderr