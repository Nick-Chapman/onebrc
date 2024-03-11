
type 'a t

val empty : unit -> 'a t
val find_opt : 'a t -> char -> 'a option
val add : 'a t -> char -> 'a -> unit
val fold : (char -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
