
module M = Map.Make(String)

type quad = { min : int; max : int; tot : int; count : int }
type t = quad M.t

let empty () = M.empty

let update name x dict =
  match M.find_opt name dict with
  | None ->
     let quad = { min = x; max = x; tot = x; count = 1 } in
     M.add name quad dict
  | Some q1 ->
     let {min=min1;max=max1;tot=tot1;count=count1} = q1 in
     let min = min min1 x in
     let max = max max1 x in
     let tot = tot1 + x in
     let count = count1 + 1 in
     M.add name {min;max;tot;count} dict

let min_mean_max {min;max;tot;count} =
  let f = Float.of_int tot /. 10. /. Float.of_int count *. 10. in
  let mean = Float.to_int (Float.floor (f +. 0.5)) in
  (min,mean,max)

let to_sorted_min_mean_max_list t =
  let f (name,quad) = (name,min_mean_max quad) in
  List.map f (M.bindings t)
