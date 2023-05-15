type t
(**Representation of a singular trade a Senator has made*)

val make : string * string * string * string -> t
(**[make (comp, tt, amt, td)] creates a value of type t*)

val company : t -> string
val transaction_type : t -> string
val amount : t -> string
val trade_date : t -> string