
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

let process lines0 =
  let rec loop dict lines = match lines with
    | [] -> dict
    | line::lines ->
       let (name,temp) = parse line in
       let quad = Quad.single temp in
       match Dict.lookup dict name with
       | None -> loop (Dict.insert dict name quad) lines
       | Some quad1 -> loop (Dict.insert dict name (Quad.combine quad1 quad)) lines

  in
  let dict0 = Dict.empty() in
  loop dict0 lines0

let show_temp : int -> string = fun i ->
  if i < 0
  then sprintf "-%d.%d" ((-i) / 10) ((-i) mod 10)
  else sprintf "%d.%d" (i / 10) (i mod 10)

let show_entry (name,quad) =
  let (min,mean,max) = Quad.min_mean_max quad in
  let z = show_temp in
  sprintf "%s=%s/%s/%s" name (z min) (z mean) (z max)

let report dict =
  match Dict.to_sorted_list dict with
  | [] -> panic "report:[]"
  | x1::xs ->
     printf "{%s" (show_entry x1);
     List.iter (fun e ->  printf ", %s" (show_entry e)) xs;
     printf "}\n"

let run() =
  let arg = Sys.argv.(1) in
  let lines = File.load (sprintf "../../data/%s.txt" arg) in
  let dict = process lines in
  report dict
