(** The [Command] module handles parsing of user commands into a variant type *)

type arg_phrase = string list
(** the type [cmd] represents the user's command and the arguments they provided *)

(** the type [field] represents all the information fields a user might request
    of a specific senator *)
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

(** [command] represents the command once parsed *)
type command =
  | Fetch of field * arg_phrase
  | List
  | Quit
  | Export of string * arg_phrase

exception Empty
(** [Empty] is raised for empty commands *)

exception Invalid
(** [Invalid] is raised for badly formatted commands *)

val parse : string -> command
(** Parses the command. A command should either be quit or list with no
    arguments (empty [arg_phrase]) or it should be a field followed by a
    senator's name *)
