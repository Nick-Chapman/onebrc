
type 'a t = 'a Trie.t

let empty : unit -> 'a t =
  fun () -> Trie.empty ()

let bindings : 'a t -> (string * 'a) list =
  fun t -> Trie.bindings t

let find_opt : 'a t -> string -> 'a option =
  fun t s ->
  let n = String.length s in
  let rec loop i c =
    if i < n then loop (i+1) (Trie.step c (String.get s i)) else
      Trie.get c
  in
  loop 0 (Trie.root t)

let add : 'a t -> string -> 'a -> unit =
  fun t s v ->
  let n = String.length s in
  let rec loop i c =
    if i < n then loop (i+1) (Trie.step c (String.get s i)) else
      Trie.set c v
  in
  loop 0 (Trie.root t)
