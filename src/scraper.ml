open Mechaml
module Monad = Agent.Monad
open Monad.Infix

exception UnknownSenator

let page_to_string (url : string) =
  Agent.get url >|= fun result -> result |> Agent.HttpResponse.content

let page_to_file (url : string) (file : string) =
  Agent.get url >|= fun result ->
  result |> Agent.HttpResponse.content |> Monad.save_content file

let action_runner (action : 'a Monad.m) : Agent.t * 'a =
  Monad.run (Agent.init ()) action

let content_to_string (url : string) : string =
  let _, st = action_runner (page_to_string url) in
  st

let content_to_file (url : string) (file : string) : unit =
  ignore (action_runner (page_to_file url file))

let ( >/> ) x f =
  match f x with
  | Some s -> s
  | None -> raise UnknownSenator

let senator_of_page (sen : string) (page : Page.t) =
  let open Soup in
  Page.soup page $$ "a[name]"
  |> filter (fun n ->
         try n >/> next_element >/> leaf_text = sen
         with UnknownSenator -> false)
  >/> first

let committees_of_senator (s : Soup.element Soup.node) =
  let open Soup in
  s >/> parent >/> next_element $$ "li"
  |> filter_map (fun n -> n $? "a")
  |> to_list
  |> List.map (fun n -> n >/> leaf_text)

let page_of_url (url : string) = Agent.get url >|= Agent.HttpResponse.page

let senate_committe_membership =
  "https://www.senate.gov/general/committee_assignments/assignments.htm"

let committee_parse sen =
  page_of_url
    "https://www.senate.gov/general/committee_assignments/assignments.htm"
  >|= fun page ->
  let open Soup in
  let senator = senator_of_page sen page in
  senator |> committees_of_senator
(* ( match committees_of_senator senator with | Some s -> print_endline s | None
   -> raise UnknownSenator) *)

(* TODO: reorganize functions, consider putting committee stuff in module *)
