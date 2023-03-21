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

module Committees : Parser
