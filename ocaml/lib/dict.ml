
type quad = {
    mutable min : int;
    mutable max : int;
    mutable tot : int;
    mutable count : int
  }

module H = Hashtbl

type t = (string,quad) H.t

let empty () = H.create 0

let update name x t =
  match H.find_opt t name with
  | None ->
     let quad = { min = x; max = x; tot = x; count = 1 } in
     H.add t name quad
  | Some q ->
     q.min <- min q.min x;
     q.max <- max q.max x;
     q.count <- q.count + 1;
     q.tot <- q.tot + x

let bindings t = H.fold (fun c n l -> (c,n) :: l) t []

let min_mean_max {min;max;tot;count} =
  let f = Float.of_int tot /. 10. /. Float.of_int count *. 10. in
  let mean = Float.to_int (Float.floor (f +. 0.5)) in
  (min,mean,max)

let to_sorted_min_mean_max_list t =
  let f (name,quad) = (name,min_mean_max quad) in
  let cmp (x,_) (y,_) = String.compare x y in
  List.sort cmp (List.map f (bindings t))
