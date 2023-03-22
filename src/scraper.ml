open Mechaml
module Monad = Agent.Monad
open Monad.Infix

exception UnknownSenator

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

  let exec () = content_to_string "url" |> parse_list
end

module Committees :
  Parser with type return = string list with type input = string = struct
  let url =
    "https://www.senate.gov/general/committee_assignments/assignments.htm"

  type return = string list
  type input = string

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
    let _, lst = action_runner (committees senator) in
    lst
end
