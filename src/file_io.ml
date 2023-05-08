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
