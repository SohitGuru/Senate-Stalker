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

open Stockinfo

let rec format_stocks_list stocks_list =
  match stocks_list with
  | [] -> []
  | h :: t ->
      [
        "Name: " ^ Stockinfo.company h;
        "Type: " ^ Stockinfo.transaction_type h;
        "Amount: " ^ Stockinfo.amount h;
        "Trade date: " ^ Stockinfo.trade_date h;
        "===";
      ]
      @ format_stocks_list t

let finance_data m =
  try
    let f =
      Scraper.FEC.exec
        (Member.last_name m ^ ", " ^ Member.first_name m
        |> String.uppercase_ascii)
    in
    let open Finance in
    [
      "Total Campaign Receipts: " ^ receipts f;
      "Total Contributions Received: " ^ total_contributions f;
      "Total Individual Contributions Received: " ^ indiv_contributions f;
    ]
  with UnknownSenator -> raise BadArgument

let export (path : string) (sen : arg_phrase) =
  let open Member in
  try
    let m = find_member sen in
    let a1, a2 = Csv_parser.Approval.exec (last_name m) in
    let d =
      [
        ("name", first_name m ^ " " ^ last_name m);
        ("party", party m);
        ("state", state m);
        ("address", address m);
        ("email", email m);
        ("phone", phone m);
        ("website", website m);
        ("class", class_num m);
        ( "committees",
          Scraper.Committees.exec (last_name m ^ ", " ^ first_name m)
          |> String.concat ", " );
        ("approval 1", a1);
        ("approval 2", a2);
        ( "nominate",
          let sen =
            (last_name m |> String.uppercase_ascii) ^ ", " ^ first_name m
          in
          Csv_parser.DWNominate.exec sen |> string_of_float );
        ("bulleted finance", "\n\t- " ^ String.concat "\n\t- " (finance_data m));
        ( "bulleted stock",
          Member.first_name m ^ " " ^ Member.last_name m |> Scraper.Stocks.exec
          |> fun lst ->
          if lst = [] then "No stocks found for this senator"
          else "\n\t- " ^ (lst |> format_stocks_list |> String.concat "\n\t- ")
        );
      ]
    in
    let originals = Markdown.list_of_file "data/template.md" in
    List.map (fun st -> Markdown.replace_snippet st d) originals
    |> Markdown.write_list path
  with UnknownSenator -> raise BadArgument

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
          with UnknownSenator -> raise BadArgument)
      | DWNom -> (
          let open Member in
          let m = find_member y in
          let sen =
            (last_name m |> String.uppercase_ascii) ^ ", " ^ first_name m
          in
          let open Csv_parser in
          try [ DWNominate.exec sen |> string_of_float; DWNominate.explanation ]
          with UnknownSenator -> raise BadArgument)
      | Finance -> finance_data (find_member y)
      | Stocks ->
          let stocks_list =
            try
              Scraper.Stocks.exec
                ( find_member y |> fun m ->
                  Member.first_name m ^ " " ^ Member.last_name m )
            with UnknownSenator -> raise BadArgument
          in
          if stocks_list = [] then [ "No trades found" ]
          else format_stocks_list stocks_list
      | Approval -> (
          let open Member in
          let m = find_member y |> last_name in
          let open Csv_parser in
          try
            let v1, v2 = Csv_parser.Approval.exec m in
            [
              "Approval Rating: " ^ v1;
              "State Partisan Leaning: " ^ v2;
              Csv_parser.Approval.explanation;
            ]
          with UnknownSenator -> raise BadArgument)
    end
  | Export (path, sen) ->
      export path sen;
      [ "Data exported to " ^ path ]
  | _ -> raise BadArgument
