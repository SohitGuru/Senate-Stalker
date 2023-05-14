let header_row (r : string) = String.split_on_char ',' r

let count_char c =
  String.fold_left (fun acc c' -> if c = c' then acc + 1 else acc) 0

let rec handle_dq_esc (lst : string list) =
  match lst with
  | a :: b :: t ->
      if count_char '\"' a mod 2 = 1 then (a ^ "," ^ b) :: t |> handle_dq_esc
      else a :: handle_dq_esc (b :: t)
  | _ -> lst

let rec clean x =
  let x =
    String.fold_left
      (fun x c -> if c = '\"' then x else x ^ String.make 1 c)
      "" x
  in
  match String.split_on_char ',' x with
  | [ last; first ] -> clean last ^ ", " ^ clean first
  | _ -> begin
      try
        let op = String.index x '(' in
        let cp = String.index x ')' in
        if op > cp then x
        else String.sub x (op + 1) (cp - (op + 1)) |> String.trim
      with Not_found -> x |> String.trim
    end

let row (r : string) (header : string list) =
  try
    List.map2
      (fun r' h' -> (h', r'))
      (String.split_on_char ',' r |> handle_dq_esc)
      header
  with Invalid_argument _ -> raise (Invalid_argument "Csv_parser.row")

let get_val (v : string) (header : string list) (r : string) =
  if List.exists (( = ) v) header then row r header |> List.assoc v |> clean
  else raise Not_found

module type Parser = sig
  val path : string

  type return
  type input

  val explanation : string
  val exec : input -> return
end

module DWNominate : Parser with type return = float with type input = string =
struct
  let path = "data/nominate.csv"

  let csv =
    lazy
      (let open Markdown in
      list_of_file path)

  let headers = lazy (header_row (List.hd (Lazy.force csv)))

  type input = string
  type return = float

  let explanation =
    "The score is on a scale from -1.0 to 1.0, where -1 is the most liberal \
     possible score and 1 is the most conservative. The Democratic Party \
     median is -0.3455 and the Republican median is 0.541."

  let exec sen =
    try
      Lazy.force csv
      |> List.find (fun x -> get_val "bioname" (Lazy.force headers) x = sen)
      |> get_val "nominate_dim1" (Lazy.force headers)
      |> float_of_string
    with Not_found -> raise Scraper.UnknownSenator
end
