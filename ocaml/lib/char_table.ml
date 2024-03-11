
module M = Map.Make(Char)

type 'a node =
  | N0
  | N1 of char * 'a
  | Node of 'a M.t

type 'a t = 'a node ref

let empty : unit -> 'a t =
  fun () -> ref N0

let find_opt : 'a t -> char -> 'a option =
  fun t char ->
  match !t with
  | N0 -> None
  | N1 (c,v) -> if char = c then Some v else None
  | Node m -> M.find_opt char m

let add : 'a t -> char -> 'a -> unit =
  fun t char v ->
  match (!t) with
  | N0 -> t := N1 (char,v)
  | N1 (c1,v1) -> t := Node (M.add char v (M.add c1 v1 M.empty))
  | Node m -> t := Node (M.add char v m)

let fold : (char -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b =
  fun f t acc ->
  match !t with
  | N0 -> acc
  | N1 (c,v) -> f c v acc
  | Node m -> M.fold f m acc
