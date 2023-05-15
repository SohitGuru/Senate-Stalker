open Mechaml
module Monad = Agent.Monad
open Monad.Infix

exception UnknownSenator

let sen_hash (sen : string) : int =
  let p1 = 53 in
  let p2 = 97 in
  String.fold_left (fun h c -> (h * p2) + Char.code c) p1 sen

let content_string_action (url : string) =
  Agent.get url >|= fun result -> result |> Agent.HttpResponse.content

let content_file_action (url : string) (file : string) =
  Agent.get url >|= fun result ->
  result |> Agent.HttpResponse.content |> Monad.save_content file

let action_runner (action : 'a Monad.m) : Agent.t * 'a =
  Monad.run (Agent.init ()) action

let content_to_string (url : string) : string =
  let _, st = action_runner (content_string_action url) in
  st

let content_to_file (url : string) (file : string) : unit =
  ignore (action_runner (content_file_action url file))

let ( >/> ) x f =
  match f x with
  | Some s -> s
  | None -> raise UnknownSenator

let page_of_url (url : string) = Agent.get url >|= Agent.HttpResponse.page

module type Parser = sig
  val url : string

  type return
  type input

  val exec : input -> return
end

module Members :
  Parser with type return = Member.t list with type input = unit = struct
  let url =
    "https://www.senate.gov/general/contact_information/senators_cfm.xml"

  type return = Member.t list
  type input = unit

  let extract_field senate_xml_list pos : string =
    List.nth (String.split_on_char '<' (List.nth senate_xml_list pos)) 0

  let make_member senate_xml_list =
    Member.make
      ( extract_field senate_xml_list 1,
        extract_field senate_xml_list 3,
        extract_field senate_xml_list 5,
        extract_field senate_xml_list 7,
        extract_field senate_xml_list 9,
        extract_field senate_xml_list 11,
        extract_field senate_xml_list 13,
        extract_field senate_xml_list 15,
        extract_field senate_xml_list 17,
        extract_field senate_xml_list 19,
        extract_field senate_xml_list 21 )

  open Member

  let rec parse_list_rec senate_xml_list acc =
    match senate_xml_list with
    | [] -> acc
    | h :: t ->
        if h = "<member" then parse_list_rec t (make_member t :: acc)
        else parse_list_rec t acc

  let parse_list senate_xml : t list =
    parse_list_rec
      (String.concat "" (senate_xml |> String.split_on_char '\n')
      |> String.split_on_char '>')
      []

  let exec () = content_to_string url |> parse_list
end

module Committees :
  Parser with type return = string list with type input = string = struct
  let url =
    "https://www.senate.gov/general/committee_assignments/assignments.htm"

  type return = string list
  type input = string

  let m : (input, return) Map.t = Map.make sen_hash 50

  open Soup

  (**[senator_of_page senator page] finds the node corresponding to [senator] in
     [page]. [senator] should be formatted last name, then a comma, then the
     first name, and then the middle initial. Raises [UnknownSenator] if there
     is an error finding the committees*)
  let senator_of_page (sen : string) (page : Page.t) =
    (* split into nodes of senator names*)
    Page.soup page $$ "a[name]"
    (* find the relevant senator in the list using filter *)
    |> filter (fun n ->
           try n >/> next_element >/> leaf_text = sen
           with UnknownSenator -> false)
    >/> first

  (** [committees_of_senator node] gives the committee list given the node
      corresponding to a senator. Raises [UnknownSenator] if not found *)
  let committees_of_senator (s : Soup.element Soup.node) =
    s >/> parent >/> next_element $$ "li"
    |> filter_map (fun n -> n $? "a")
    |> to_list
    |> List.map (fun n -> n >/> leaf_text)

  (** [committees sen] is the action function that gets the committee
      assignments of a given senator ([sen]). It will raise [UnknownSenator] if
      there's an issue finding the senator and/or parsing their information.*)
  let committees sen =
    page_of_url url >|= fun page ->
    let senator = senator_of_page sen page in
    senator |> committees_of_senator

  (** [exec senator] returns a string list of the committee asignments of
      Senator [senator]. [senator] should be formatted last name, then a comma,
      then the first name, and then the middle initial. Raises [UnknownSenator]
      if there is an error finding the senator or their committees. s*)
  let exec senator =
    try Map.get senator m
    with Not_found ->
      let _, ret = action_runner (committees senator) in
      Map.put senator ret m;
      ret
end

module FEC : Parser with type return = Finance.t with type input = string =
struct
  open Soup

  type input = string
  type return = Finance.t

  let url = "https://www.fec.gov"
  let search = "/data/search/?search="
  let map : (input, return) Map.t = Map.make sen_hash 50
  (* used to store results since it's hard to compute*)

  let search sen =
    let open Page in
    let spaces_handled = String.split_on_char ' ' sen |> String.concat "+" in
    Agent.get (url ^ search ^ spaces_handled) >|= fun res ->
    res |> Agent.HttpResponse.page |> links_with "a"
    |> filter (fun x ->
           match Link.text x with
           | Some s -> s = sen
           | None -> false)
    |> filter (fun x -> String.get (Link.href x) 16 = 'S')
    >/> first |> Link.href

  let fetch_total_receipts s =
    s $ "span[data-term=\"total receipts\"]" >/> parent >/> next_element
    >/> leaf_text |> String.trim

  let fetch_total_contributions s =
    s $ "td:contains(\"Total contributions\")" >/> next_element >/> leaf_text
    |> String.trim

  let fetch_indiv_contributions s =
    s $ "td:contains(\"Total individual contributions\")" >/> next_element
    >/> leaf_text |> String.trim

  let fetch data =
    let url = url ^ data ^ "/" in
    Agent.get url >|= fun res ->
    let s = res |> Agent.HttpResponse.page |> Page.soup in
    ( fetch_total_receipts s,
      fetch_total_contributions s,
      fetch_indiv_contributions s )

  let exec senator =
    try Map.get senator map
    with Not_found ->
      let _, d = action_runner (search senator) in
      let _, tup = action_runner (fetch d) in
      let ret = Finance.make tup in
      Map.put senator ret map;
      ret
end

open Stockinfo

module Stocks :
  Parser with type return = Stockinfo.t list with type input = string = struct
  let url = "https://www.quiverquant.com/congresstrading/politician/"

  type return = Stockinfo.t list
  type input = string

  open Soup

  (*[fill_spaces] takes an input string and returns that string with "%20"
    replacing every space*)
  let rec fill_spaces_rec lst =
    match lst with
    | [] -> ""
    | h :: t -> h ^ "%20" ^ fill_spaces_rec t

  let fill_spaces str =
    String.split_on_char ' ' str |> fill_spaces_rec |> fun x ->
    String.sub x 0 (String.length x - 3)

  let rec remove_leading_spaces str =
    if String.length str >= 1 && (str.[0] = ' ' || str.[0] = '\n') then
      remove_leading_spaces (String.sub str 1 (String.length str - 1))
    else str

  let rec remove_trailing_spaces str =
    let str_length = String.length str in
    if
      str_length >= 1
      && (str.[str_length - 1] = ' ' || str.[str_length - 1] = '\n')
    then remove_trailing_spaces (String.sub str 0 (String.length str - 1))
    else str

  let eval_node nd =
    let spans = nd $$ "span" |> to_list in
    let spans_eval span_num =
      (match List.nth spans span_num |> leaf_text with
      | None -> ""
      | Some s -> s)
      |> remove_leading_spaces |> remove_trailing_spaces
      (*remove_leading_spaces (List.nth (String.split_on_char '\n' (List.nth
        spans span_num |> to_string)) 1)*)
    in
    let span_const = if spans_eval 0 <> "-" then 1 else 0 in
    let company =
      spans_eval (1 + span_const)
      ^ ", which is an asset of type "
      ^ spans_eval (2 + span_const)
    in
    let transaction_type = spans_eval (3 + span_const) in
    let amount = spans_eval (4 + span_const) in
    let a_list = nd $$ "a" |> to_list in
    let trade_date =
      remove_leading_spaces
        (List.nth
           (String.split_on_char '\n' (List.nth a_list 4 |> to_string))
           1)
    in
    Stockinfo.make (company, transaction_type, amount, trade_date)

  let rec make_trades_list_rec tbody_filtered =
    match tbody_filtered with
    | [] -> []
    | h :: t -> eval_node h :: make_trades_list_rec t

  let make_trades_list senator_page =
    let tbody = senator_page $ "tbody" in
    let tbody_filtered =
      List.filter
        (fun x -> String.sub (x |> to_string) 0 1 <> "\n")
        (tbody |> children |> to_list)
    in
    make_trades_list_rec tbody_filtered

  let rec first_name str =
    match str with
    | "" -> ""
    | _ -> (
        let fl = String.sub str 0 1 in
        match fl with
        | " " -> ""
        | _ -> fl ^ first_name (String.sub str 1 (String.length str - 1)))

  let rec last_name str =
    match str with
    | "" -> ""
    | _ -> (
        let str_length = String.length str in
        let ll = String.sub str (str_length - 1) 1 in
        match ll with
        | " " -> ""
        | _ -> last_name (String.sub str 0 (str_length - 1)) ^ ll)

  let rec remove_mi str = first_name str ^ " " ^ last_name str

  let trades_attempt senator =
    let formatted_url = url ^ fill_spaces senator ^ "?" in
    let senator_page = page_of_url formatted_url >|= Page.soup in
    senator_page >|= fun x ->
    if
      x $? "p:contains(\"No trading activity found for this politician.\")"
      <> None
    then []
    else make_trades_list x

  let trades senator =
    let possible_mi = trades_attempt senator in
    let _, lst = action_runner possible_mi in
    if lst <> [] then lst
    else
      let _, lst2 = action_runner (trades_attempt (remove_mi senator)) in
      lst2

  let exec senator = trades senator
end
