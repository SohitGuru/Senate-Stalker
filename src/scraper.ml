open Mechaml
module Monad = Agent.Monad
open Monad.Infix

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
