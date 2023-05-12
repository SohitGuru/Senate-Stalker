let rec read_list (inc : in_channel) (acc : string list) =
  try
    let lst = input_line inc :: acc in
    read_list inc lst
  with End_of_file ->
    close_in inc;
    acc

let list_of_file f = read_list (open_in f) []
let string_of_file f = String.concat "\n" (list_of_file f)

let write_str f s =
  let out = open_out f in
  output_string out s;
  close_out out

let write_list f lst = String.concat "\n" lst |> write_str f

let handle_part st open_delim d : string =
  match String.split_on_char open_delim st with
  | [ preface; token ] -> (
      try preface ^ Dictionary.find token d
      with Dictionary.NotFound -> preface ^ st)
  | _ -> failwith "Bad input"

let replace_snippet ?(open_delim = '{') ?(close_delim = '}') (o : string)
    (d : (string, string) Dictionary.t) : string =
  let lst = String.split_on_char close_delim o in
  let revlist = List.rev lst in
  let replaced =
    List.map (fun x -> handle_part x open_delim d) (revlist |> List.tl)
  in
  String.concat "" (List.hd revlist :: replaced |> List.rev)
