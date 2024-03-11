
open Printf

(*module Dict = Dict_built_on_trie*)
module Dict = Dict_orig

exception Panic of string
let panic s = raise (Panic s)

let code0 = Char.code '0'

(* This IS better than input_char *)
let make_get_char ic =
  let buf_size = 65536 in
  let buf = Bytes.create buf_size in
  let p = ref 0 in
  let max = ref 0 in
  let refill() =
    let n = input ic buf 0 buf_size in
    (*if (n != buf_size) then printf "refill-> %d\n" n;*)
    if (n = 0) then raise End_of_file;
    max := n;
    p := 0;
    ()
  in
  let get_char () =
    if !p == !max then refill();
    let c = Bytes.get buf (!p) in
    incr p;
    (*let () = printf "v2-inp(%d):%c\n" (!remain) c in*)
    c
  in
  get_char

let make_get_name get_char =
  let buf = Buffer.create 80 in
  let rec line() =
    let c = get_char() in
    if c = ';' then () else
      let () = Buffer.add_char buf c in
      line()
  in
  let get_line () =
    Buffer.clear buf;
    match try Some (get_char()) with End_of_file -> None with
    | None -> None
    | Some c ->
       let () = Buffer.add_char buf c in
       line();
       Some (Buffer.contents buf)
  in
  get_line

let make_get_temp get_char =
  let looking_for_frac acc =
    let c = get_char() in
    let d = Char.code c - code0 in
    10 * acc + d
  in
  let rec looking_for_whole acc =
    let c = get_char() in
    if c = '.' then looking_for_frac acc else
      let d = Char.code c - code0 in
      let acc' = 10 * acc + d in
      looking_for_whole acc'
  in
  let looking_for_number() =
    let c = get_char() in
    if c = '-' then - (looking_for_whole 0 ) else
      let d = Char.code c - code0 in
      looking_for_whole d
  in
  fun () ->
  let n = looking_for_number() in
  let _x = get_char() in
  (*if _x != '\n' then panic "expecting NL";*)
  n

type quad = {
    mutable min : int;
    mutable max : int;
    mutable tot : int;
    mutable count : int
  }

let update name x t =
  match Dict.find_opt t name with
  | None ->
     let quad = { min = x; max = x; tot = x; count = 1 } in
     Dict.add t name quad
  | Some q ->
     q.min <- min q.min x;
     q.max <- max q.max x;
     q.count <- q.count + 1;
     q.tot <- q.tot + x

let process filename =
  let ic = open_in filename in
  let get_char = make_get_char ic in
  let get_name = make_get_name get_char in
  let get_temp = make_get_temp get_char in
  let dict = Dict.empty() in
  let rec loop() =
    match get_name() with
    | None -> ()
    | Some name ->
       let temp = get_temp() in
       update name temp dict;
       loop()
  in
  let () = loop() in
  let () = close_in ic in
  dict

let show_temp : int -> string = fun i ->
  if i < 0
  then sprintf "-%d.%d" ((-i) / 10) ((-i) mod 10)
  else sprintf "%d.%d" (i / 10) (i mod 10)

let show_entry (name,(min,mean,max)) =
  let z = show_temp in
  sprintf "%s=%s/%s/%s" name (z min) (z mean) (z max)

let min_mean_max {min;max;tot;count} =
  let f = Float.of_int tot /. 10. /. Float.of_int count *. 10. in
  let mean = Float.to_int (Float.floor (f +. 0.5)) in
  (min,mean,max)

let to_sorted_min_mean_max_list t =
  let f (name,quad) = (name,min_mean_max quad) in
  let cmp (x,_) (y,_) = String.compare x y in
  List.sort cmp (List.map f (Dict.bindings t))

let report dict =
  match to_sorted_min_mean_max_list dict with
  | [] -> panic "report:[]"
  | e1::xs ->
     printf "{%s" (show_entry e1);
     List.iter (fun e ->  printf ", %s" (show_entry e)) xs;
     printf "}\n"

let run() =
  let arg = Sys.argv.(1) in
  let filename = sprintf "../data/%s.txt" arg in
  let dict = process filename in
  report dict
