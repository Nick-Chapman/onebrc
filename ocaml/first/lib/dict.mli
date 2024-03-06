
type t

val empty : unit -> t
val lookup : t -> string -> Quad.t option
val insert : t -> string -> Quad.t -> t
val to_sorted_list : t -> (string * Quad.t) list
