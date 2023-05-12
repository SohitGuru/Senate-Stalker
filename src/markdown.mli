val list_of_file : string -> string list
(** [list_of_file path] reads the file line-by-line into a list of strings
    (where each item is a line), which is returned. *)

val string_of_file : string -> string
(** [string_of_file path] reads the file at path into one (potentially large)
    string. *)

val write_str : string -> string -> unit
(** [write_str path text] writes a string [text] to a file at [path]. If the
    file doesn't exist, it is created. If the file does exist, it is
    overrwritten. *)

val write_list : string -> string list -> unit
(** [write_list path lst] behaves similary to [write_str path text], but takes a
    list of lines instead of a singular stirng to be written. The list is
    concatenated with a newline between each entry and then written to the file
    at [path]. *)

val replace_snippet :
  ?open_delim:char ->
  ?close_delim:char ->
  string ->
  (string, string) Dictionary.t ->
  string
(** [replace_snippet ?open_delim:o ?close_delim:c original dictionary] replaces
    a series of delimited tokens with given values. [dictionary] should map each
    token to the value that should replace it. For each binding of a [token] to
    a [replacement] in the map, every occurance of [o ^ token ^ c] in [original]
    with [replacement]. By default, the open delimiter is ['{'] and the close
    delimiter is ['}']. *)
