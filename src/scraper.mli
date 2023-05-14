exception UnknownSenator

val content_to_string : string -> string
(** [content_to_string url] fetches the content of the webpage at [url] and
    returns it as a string *)

val content_to_file : string -> string -> unit
(** [content_to_file url file] saves the content of the webpage at [url] and
    saves it to [file] *)

module type Parser = sig
  val url : string

  type return
  type input

  val exec : input -> return
end

module Members : Parser with type return = Member.t list with type input = unit

module Committees :
  Parser with type return = string list with type input = string
(** [Committees.exec senator] returns a string list of the committee asignments
    of Senator [senator]. [senator] should be formatted last name, then a comma,
    then the first name, and then the middle initial. Raises [UnknownSenator] if
    there is an error finding the senator or their committees.*)
