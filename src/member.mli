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
(** [make (full, last_name, first_name, party,
    state, address, phone, email, website, class_num, biograhpical_id)]
    creates a value of type t *)

val full_name : t -> string
(** [full_name m] gets the full name of a member [m]*)

val last_name : t -> string
(** [last_name m] gets the last name of a member [m]*)

val first_name : t -> string
(** [first_name m] gets the first name of a member [m] *)

val party : t -> string
(** [party m] gets the party of a member [m]. Note: the returned value is a
    single-letter abbreviation *)

val state : t -> string
(** [state m] gets the state of a member [m]. Note: the returned value is a
    two-letter abbreviation *)

val address : t -> string
(** [address m] gets the office address of a member [m] *)

val phone : t -> string
(** [address phone] gets the office phone of a member [m] *)

val email : t -> string
(** [email m] gets the email of a member [m] *)

val website : t -> string
(** [website m] gets the website of a member [m]*)

val class_num : t -> string
(** [class_num m] gets the electoral class number of a member [m] *)

val biographical_id : t -> string
(** [biographical_id m] gets the biographical id of a member [m] *)
