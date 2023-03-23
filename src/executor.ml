exception BadArgument
exception UnexpectedError

open Command
open Scraper

let find_member (name : string list) =
  String.concat " " name |> print_endline;
  match name with
  | last :: first :: _ -> (
      match
        Scraper.Members.exec ()
        |> List.find_opt (fun m ->
               let open Member in
               first_name m = first && last_name m = last)
      with
      | Some m -> m
      | None -> raise BadArgument)
  | _ -> raise BadArgument

let execute (cmd : command) =
  match cmd with
  | List ->
      Scraper.Members.exec ()
      |> List.map (fun member ->
             let open Member in
             first_name member ^ " " ^ last_name member)
  | Fetch (x, y) -> (
      match x with
      | Name -> y (* name is what we receive as our input *)
      | Party -> [ find_member y |> Member.party ]
      | State -> [ find_member y |> Member.state ]
      | Address -> [ find_member y |> Member.address ]
      | Phone -> [ find_member y |> Member.phone ]
      | Email -> [ find_member y |> Member.email ]
      | Website -> [ find_member y |> Member.website ]
      | Class -> [ find_member y |> Member.class_num ]
      | Committees -> Scraper.Committees.exec (String.concat ", " y))
  | _ -> raise BadArgument
