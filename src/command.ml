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

let rec remove_leading_spaces str =
  if String.length str >= 1 && str.[0] = ' ' then
    remove_leading_spaces (String.sub str 1 (String.length str - 1))
  else str

(*Returns the index of the next space in the string, returns the length of str
  if no space is present*)
let rec next_space_index_rec str indx =
  if str = "" || str.[0] = ' ' then indx
  else next_space_index_rec (String.sub str 1 (String.length str - 1)) (indx + 1)

(*Calls next_space_index_rec with proper format*)
let next_space_index str = next_space_index_rec str 0

let fetch_field str =
  let ns_str = str |> remove_leading_spaces in
  match String.sub ns_str 0 (next_space_index ns_str) with
  | "Name" -> Name
  | "Party" -> Party
  | "State" -> State
  | "Address" -> Address
  | "Phone" -> Phone
  | "Email" -> Email
  | "Website" -> Website
  | "Class" -> Class
  | "Committees" -> Committees
  | _ -> raise Invalid

let rec fetch_args_rec str_list acc =
  match str_list with
  | [] -> acc
  | h :: t ->
      if h = "" then fetch_args_rec t acc else fetch_args_rec t (h :: acc)

let fetch_args str =
  let ns_str = str |> remove_leading_spaces in
  let str_commands =
    String.sub ns_str (next_space_index ns_str) (String.length ns_str - 1)
  in
  fetch_args_rec (String.split_on_char ' ' str_commands) []

let rec parse str =
  let str_next_space = next_space_index str in
  let str_spaceless = str |> remove_leading_spaces in
  let word = String.sub str_spaceless 0 str_next_space in
  let str_tail =
    String.sub str_spaceless str_next_space (String.length str_spaceless - 1)
    |> remove_leading_spaces
  in
  if word = "List" && str_tail = "" then List
  else if word = "Quit" && str_tail = "" then Quit
  else if word = "Fetch" then Fetch (fetch_field str_tail, fetch_args str_tail)
  else raise Invalid
