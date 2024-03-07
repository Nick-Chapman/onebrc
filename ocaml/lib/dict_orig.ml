
module H = Hashtbl

type 'a t = (string,'a) H.t

let empty () = H.create 0
let find_opt t name = H.find_opt t name
let add t name quad = H.add t name quad
let bindings t = H.fold (fun c n l -> (c,n) :: l) t []
