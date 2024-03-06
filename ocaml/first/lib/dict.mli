
type t

val empty : unit -> t
val update : string -> int -> t -> t
val to_sorted_min_mean_max_list : t -> (string * (int * int * int)) list