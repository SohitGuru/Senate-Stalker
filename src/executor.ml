exception BadArgument
exception UnexpectedError

open Command
open Scraper

let find_member (name : string list) =
  match name with
  | lst -> (
      match
        Scraper.Members.exec ()
        |> List.find_opt (fun m ->
               let open Member in
               first_name m ^ " " ^ last_name m
               = String.concat " " (List.rev lst))
      with
      | Some m -> m
      | None -> raise BadArgument)

let export (path : string) (sen : arg_phrase) =
  let open Member in
  let m = find_member sen in
  let d =
    Dictionary.(
      empty
      |> insert "name" (first_name m ^ " " ^ last_name m)
      |> insert "party" (party m)
      |> insert "state" (state m)
      |> insert "address" (address m)
      |> insert "email" (email m)
      |> insert "phone" (phone m)
      |> insert "website" (website m)
      |> insert "class" (class_num m)
      |> insert "committees"
           (try
              Scraper.Committees.exec (last_name m ^ ", " ^ first_name m)
              |> String.concat ", "
            with UnknownSenator -> raise UnexpectedError))
  in
  let originals = Markdown.list_of_file "data/template.md" in
  List.map (fun st -> Markdown.replace_snippet st d) originals
  |> List.rev |> Markdown.write_list path

let execute (cmd : command) =
  match cmd with
  | List ->
      Scraper.Members.exec ()
      |> List.map (fun member ->
             let open Member in
             first_name member ^ " " ^ last_name member)
  | Fetch (x, y) -> begin
      match x with
      | Name ->
          [
            ( find_member y |> fun m ->
              Member.last_name m ^ ", " ^ Member.first_name m );
          ]
          (* name is what we receive as our input *)
      | Party -> [ find_member y |> Member.party ]
      | State -> [ find_member y |> Member.state ]
      | Address -> [ find_member y |> Member.address ]
      | Phone -> [ find_member y |> Member.phone ]
      | Email -> [ find_member y |> Member.email ]
      | Website -> [ find_member y |> Member.website ]
      | Class -> [ find_member y |> Member.class_num ]
      | Committees -> (
          try
            Scraper.Committees.exec
              ( find_member y |> fun m ->
                Member.last_name m ^ ", " ^ Member.first_name m )
          with UnknownSenator -> raise UnexpectedError)
      | DWNom ->
          let open Member in
          let m = find_member y in
          let sen =
            (last_name m |> String.uppercase_ascii) ^ ", " ^ first_name m
          in
          let open Csv_parser in
          [ DWNominate.exec sen |> string_of_float; DWNominate.explanation ]
    end
  | Export (path, sen) ->
      export path sen;
      [ "Data exported to " ^ path ]
  | _ -> raise BadArgument
