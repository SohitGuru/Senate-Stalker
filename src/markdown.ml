let rec read_list (inc : in_channel) (acc : string list) =
  try
    let lst = input_line inc :: acc in
    read_list inc lst
  with End_of_file ->
    close_in inc;
    acc

let list_of_file f = read_list (open_in f) [] |> List.rev
let string_of_file f = String.concat "\n" (list_of_file f)

let write_str f s =
  let out = open_out f in
  output_string out s;
  close_out out

let write_list f lst = String.concat "\n" lst |> write_str f

let handle_part st open_delim d : string =
  if String.contains st open_delim then
    match String.split_on_char open_delim st with
    | [ preface; token ] -> (
        try preface ^ List.assoc token d with Not_found -> preface ^ st)
    | _ -> failwith "Bad input: " ^ st
  else st

let replace_snippet ?(open_delim = '{') ?(close_delim = '}') (o : string)
    (d : (string * string) list) : string =
  match String.split_on_char close_delim o with
  | [] | [ _ ] -> o
  | h :: _ as lst ->
      let replaced = List.map (fun x -> handle_part x open_delim d) lst in
      String.concat "" replaced
