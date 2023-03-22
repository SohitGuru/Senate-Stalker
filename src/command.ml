type arg_phrase = string list

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

type command =
  | Fetch of field * arg_phrase
  | List
  | Quit

exception Empty
exception Invalid

let parse str = failwith "Unimplemented" (* TODO *)
