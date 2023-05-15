(**Representation of a singular trade a Senator has made*)

type t
(**Abstract type representing stock trades*)

val make : string * string * string * string -> t
(**[make (comp, tt, amt, td)] creates a value of type t*)

(**[company t] returns the company of a value of type t*)
val company : t -> string
(**[transaction_type t] returns the transaction_type of a value of type t*)
val transaction_type : t -> string
(**[amount t] returns the amount of a value of type t*)
val amount : t -> string
(**[trade_date t] returns the trade_date of a value of type t*)
val trade_date : t -> string
