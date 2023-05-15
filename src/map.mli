(** [Map] is a mutable map module that accepts any key/value types. *)

type ('k, 'v) t
(** [('k, 'v) t] is the type of a mutable map that creates bindings from keys of
    type ['k] to values of type ['v] *)

val make : ('k -> int) -> int -> ('k, 'v) t
(** [make hash capacity] creates a table with the hash function [hash] and
    initial capacity [capacity]. The hash function should map an object of type
    ['k] to an [int], and is responsible for diffusion and serialization.

    Raises [Invalid_argument] if [c <= 0] *)

val size : ('k, 'v) t -> int
(** [size m] gets the number of bindings in the map *)

val put : 'k -> 'v -> ('k, 'v) t -> unit
(** [put k v m] put the binding of key [k] to value [v] to the map [m]. If [k]
    is already bound to some value [v'] in m, the binding is replaced with the
    given value [v] *)

val get : 'k -> ('k, 'v) t -> 'v
(** [get k m] gets the value (type ['v]) bound to [k] in [m]. Raises [Not_found]
    if [k] is not bound to a value in [m] *)

val remove : 'k -> ('k, 'v) t -> unit
(** [remove k m] removes the binding of key [k] from map [m]. Nothing occurs if
    the key is not bound to anything in [m] *)

val bindings : ('k, 'v) t -> ('k * 'v) list
(** [bindings m] returns [m] represented as an association list. No specific
    ordering of the items in the returned list is guaranteed. *)
