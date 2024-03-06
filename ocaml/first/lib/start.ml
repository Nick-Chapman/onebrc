
open Printf

exception Panic of string
let panic s = raise (Panic s)

let parse : string -> (string*int) =
  fun s ->
  match String.split_on_char ';' s with
  | [name;num] ->
     (match String.split_on_char '.' num with
      | [whole;frac] ->
         let f = int_of_string frac in
         let mm = String.get whole 0 in
         if mm = '-'
         then
           let whole' = String.sub whole 1 (String.length whole - 1) in
           let w = int_of_string whole' in
           (name, - (10 * w + f))
         else
           let w = int_of_string whole in
           (name, 10 * w + f)
      | _ -> panic "parse:split2";
     )
  | _ -> panic "parse:split1"

let process filename =
  let ic = open_in filename in
  let rec loop dict =
    match try Some (input_line ic) with End_of_file -> None with
    | None -> dict
    | Some line ->
       let (name,temp) = parse line in
       loop (Dict.update name temp dict)
  in
  let dict0 = Dict.empty() in
  let dict = loop dict0 in
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
  let filename = sprintf "../../data/%s.txt" arg in
  let dict = process filename in
  report dict
