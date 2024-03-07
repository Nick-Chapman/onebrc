
module H = Hashtbl

type 'a cursor = { mutable opt: 'a option; link: (char,'a cursor) H.t }

type 'a t = 'a cursor

let empty : unit -> 'a t =
  fun () -> { opt = None; link = H.create 0 }

let implode : char list -> string =
  fun xs -> String.concat "" (List.map (String.make 1) xs)

let bindings : 'a t -> (string * 'a) list =
  fun t ->
  let rec collect xs t acc =
    let acc = H.fold (fun x c acc -> collect (x::xs) c acc) t.link acc in
    match t.opt with
    | None -> acc
    | Some v -> (implode (List.rev xs), v) :: acc
  in
  collect [] t []

let root : 'a t -> 'a cursor =
  fun t -> t

let step : 'a cursor -> char -> 'a cursor =
  fun t char ->
  match H.find_opt t.link char with
  | None ->
     let e = empty() in
     H.add t.link char e;
     e
  | Some c -> c

let get : 'a cursor -> 'a option =
  fun c ->
  c.opt

let set : 'a cursor -> 'a -> unit =
  fun c v ->
  c.opt <- Some v
