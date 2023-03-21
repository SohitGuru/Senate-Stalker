open OUnit2
open Senate
open Scraper

let senate_members_url =
  "https://www.senate.gov/general/contact_information/senators_cfm.xml"

let scraper_tests =
  [
    ( "Senate members xml first line test" >:: fun _ ->
      assert_equal
        (content_to_string senate_members_url
        |> String.split_on_char '\n' |> List.hd)
        "<?xml version=\"1.0\" \
         encoding=\"UTF-8\"?><contact_information><member>" );
  ]

let parser_tests = []

(* let x = content_to_file "" "" *)
let gui_tests = []

let tests =
  "test suite for project"
  >::: List.flatten [ scraper_tests; parser_tests; gui_tests ]

let _ = run_test_tt_main tests
