
let %hashcons l = []

let %hashcons one = 1

let rec create_list n =
    if n > 0
    then 1 :: create_list (n - 1)
    else []

let list = create_list 100
let oc = open_out_gen [Open_creat; Open_append] 0o666 "Results/mem_results.txt"
let mem_used = Gc.allocated_bytes ()
let () = Printf.fprintf oc "Number of bytes allocated: %f MB\n" (mem_used/.1024.0/.1024.0)