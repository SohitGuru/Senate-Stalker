exception UnknownSenator

val content_to_string : string -> string
(** [content_to_string url] fetches the content of the webpage at [url] and
    returns it as a string *)

val content_to_file : string -> string -> unit
(** [content_to_file url file] saves the content of the webpage at [url] and
    saves it to [file] *)

(** A module of type [Parser] is in charge of taking some value of type [input]
    and scraping information from the website at [url] to yield a target value
    of type [return]. *)
module type Parser = sig
  val url : string

  type return
  type input

  val exec : input -> return
end

module Members : Parser with type return = Member.t list with type input = unit
(** [Members.exec ()] returs a list of objects of type [Member.t]. Each senator
    has an entry in this list. Their entry contains basic biographical
    information detailed in the [Member] compilation unit. *)

module Committees :
  Parser with type return = string list with type input = string
(** [Committees.exec senator] returns a string list of the committee asignments
    of Senator [senator]. [senator] should be formatted last name, then a comma,
    then the first name, and then the middle initial. Raises [UnknownSenator] if
    there is an error finding the senator or their committees. *)

module FEC : Parser with type return = Finance.t with type input = string
(** [FEC.exec senator] returns an object of [Finance.t], which contains the
    campaign finance information for some senator [senator]. [senator] should be
    formatted last name, then a comma, then the first name, and then the middle
    initial. Moreover, the [senator] string should be in all caps.

    Raises [UnknownSenator] if there is an error finding the senator or their
    campaign finance data. *)

module Stocks :
  Parser with type return = Stockinfo.t list with type input = string
(** [Stocks.exec senator] returns a string list of all the recorded stock trades
    of a Senator [senator]. [senator] should be in the form of "[first name]
    [middle initial] [last name]". Returns the empty list if no trades have been
    logged for that specific senator.*)
