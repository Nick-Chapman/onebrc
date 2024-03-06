
open Printf

exception Panic of string
let panic s = raise (Panic s)

let code0 = Char.code '0'

let process_line line dict =
  let looking_for_frac acc n =
    let c = String.get line n in
    let d = Char.code c - code0 in
    10 * acc + d
  in
  let rec looking_for_whole acc n =
    let c = String.get line n in
    if c = '.' then looking_for_frac acc (n+1) else
      let d = Char.code c - code0 in
      let acc' = 10 * acc + d in
      looking_for_whole acc' (n+1)
  in
  let looking_for_number n =
    let c = String.get line n in
    if c = '-' then - (looking_for_whole 0 (n+1)) else
      let d = Char.code c - code0 in
      looking_for_whole d (n+1)
  in
  let rec looking_for_name n =
    let c = String.get line n in
    if c <> ';' then looking_for_name (n+1) else
      let name = String.sub line 0 n in
      let temp = looking_for_number (n+1) in
      Dict.update name temp dict
  in
  looking_for_name 0

let process filename =
  let ic = open_in filename in
  let dict = Dict.empty() in
  let rec loop() =
    match try Some (input_line ic) with End_of_file -> None with
    | None -> ()
    | Some line -> (process_line line dict; loop())
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

let report dict =
  match Dict.to_sorted_min_mean_max_list dict with
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
