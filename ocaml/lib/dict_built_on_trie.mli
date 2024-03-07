
(* same interface as dict.mli *)

type 'a t

val empty : unit -> 'a t
val bindings : 'a t -> (string * 'a) list
val find_opt : 'a t -> string -> 'a option
val add : 'a t -> string -> 'a -> unit
