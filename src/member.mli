(** Representation of a member and their basic information *)

type t
(** abstract type representing members *)

val make :
  string
  * string
  * string
  * string
  * string
  * string
  * string
  * string
  * string
  * string
  * string ->
  t

val full_name : t -> string
val last_name : t -> string
val first_name : t -> string
val party : t -> string
val state : t -> string
val address : t -> string
val phone : t -> string
val email : t -> string
val website : t -> string
val class_num : t -> string
val biographical_id : t -> string
