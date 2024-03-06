
module M = Map.Make(String)

type t = Quad.t M.t

let empty () = M.empty
let lookup t k = M.find_opt k t
let insert t k q = M.add k q t
let to_sorted_list t = M.bindings t
