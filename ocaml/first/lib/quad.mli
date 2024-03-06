
type t

val single : int -> t
val combine : t -> t -> t
val min_mean_max : t -> int * int * int
