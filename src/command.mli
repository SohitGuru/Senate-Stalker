(** Parsing of user commands *)

type arg_phrase = string list
(** the type [cmd] represents the user's command and the arguments they provided *)

type field =
  | Name
  | Party
  | State
  | Address
  | Phone
  | Email
  | Website
  | Class
  | Committees
      (** the type [field] represents all the information fields a user might
          request of a specific senator *)

type command =
  | Fetch of field * arg_phrase
  | List
  | Quit

exception Empty
exception Invalid

val parse : string -> command
(** Parses the command. A command should either be quit or list with no
    arguments (empty [arg_phrase]) or it should be a field followed by a
    senator's name *)
