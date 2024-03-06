
module M = Map.Make(String)

type quad = {
    mutable min : int;
    mutable max : int;
    mutable tot : int;
    mutable count : int
  }

type t = { mutable dict : quad M.t }

let empty () = { dict = M.empty }

let update name x t =
  match M.find_opt name t.dict with
  | None ->
     let quad = { min = x; max = x; tot = x; count = 1 } in
     t.dict <- M.add name quad t.dict
  | Some q ->
     q.min <- min q.min x;
     q.max <- max q.max x;
     q.count <- q.count + 1;
     q.tot <- q.tot + x;
     ()

let min_mean_max {min;max;tot;count} =
  let f = Float.of_int tot /. 10. /. Float.of_int count *. 10. in
  let mean = Float.to_int (Float.floor (f +. 0.5)) in
  (min,mean,max)

let to_sorted_min_mean_max_list {dict} =
  let f (name,quad) = (name,min_mean_max quad) in
  List.map f (M.bindings dict)
