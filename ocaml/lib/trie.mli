
type 'a t
type 'a cursor

val empty : unit -> 'a t
val bindings : 'a t -> (string * 'a) list

val root : 'a t -> 'a cursor
val step : 'a cursor -> char -> 'a cursor
val get : 'a cursor -> 'a option
val set : 'a cursor -> 'a -> unit
