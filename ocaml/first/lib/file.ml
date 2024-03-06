
let load filename =
  let ic = open_in filename in
  let rec loop acc =
    match try Some (input_line ic) with End_of_file -> None with
    | Some line  -> loop (line::acc)
    | None -> List.rev acc
  in
  let res = loop [] in
  let () = close_in ic in
  res
