type ('k, 'v) t
(** This is the representative type for the dictionary. ['k] is the key type and
    ['v] is the value type for every binding in the dictionary*)

exception NotFound

val empty : ('k, 'v) t
(** [empty] represents a dictionary with no mappings/bindings *)

val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
(** [insert k v d] adds the binding of key [k] to value [v] in the dictionary
    [m] *)

val find : 'k -> ('k, 'v) t -> 'v
(** [find k d] finds the value for key [k] in dictionary [d]. If [k] is not
    bound to a value in [d], [NotFound] is raised *)

val to_list : ('k, 'v) t -> ('k * 'v) list
(** [to_list d] returns the association list form of the dictionary. This list
    will be sorted in ascending order using the default OCaml operators [<],
    [>], and [=]. There will be no two [(k1, v1)] and [(k2, v2)] in the list
    such that [k1 = k2]. *)
