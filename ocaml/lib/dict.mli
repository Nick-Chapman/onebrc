
type 'a t

val empty : unit -> 'a t
val find_opt : 'a t -> string -> 'a option
val add : 'a t -> string -> 'a -> unit
val bindings : 'a t -> (string * 'a) list
