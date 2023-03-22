exception BadArgument
exception UnexpectedError

open Command
open Scraper

let execute (cmd : command) =
  match cmd with
  | Quit -> exit 0
  | List -> Scraper.Members.exec () |> List.map
  | Fetch (x, y) -> (
      match x with
      | Name -> [ "" ]
      | Party -> [ "" ]
      | State -> [ "" ]
      | Address -> [ "" ]
      | Phone -> [ "" ]
      | Email -> [ "" ]
      | Website -> [ "" ]
      | Class -> [ "" ]
      | Committees -> Scraper.Committees.exec (String.concat "" y)
      | _ -> raise BadArgument)
