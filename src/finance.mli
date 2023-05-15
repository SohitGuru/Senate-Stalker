(** Representation type of campaign finance data for a senator *)

type t

val make : string * string * string -> t
(** [make (a, b, c)] makes a campaign finance entry where the total receipts
    value is [a], the total contributions value is [b], and the individual
    contributions value is [c]. *)

val receipts : t -> string
(** [receipts t] is the dollar amount of total receipts spent, as per this
    entry. *)

val total_contributions : t -> string
(** [receipts t] is the dollar amount of total contributions, as per this entry. *)

val indiv_contributions : t -> string
(** [receipts t] is the dollar amount of all individual contributions, as per
    this entry. *)
