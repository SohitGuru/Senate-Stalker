open OUnit2
open Senate
open Scraper
open Command
open Executor

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

let markdown_tests =
  let open Markdown in
  [
    ( "basic snippet replacement tests" >:: fun _ ->
      assert_equal (replace_snippet "{name}" "name" "Gorg Abbott") "Gorg Abbott"
    );
    ( "embedded snippet test" >:: fun _ ->
      assert_equal
        (replace_snippet "Lorem {word} dolor sit amet" "word" "ipsum")
        "Lorem ipsum dolor sit amet" );
    ( "multiple identical tokens" >:: fun _ ->
      assert_equal
        (replace_snippet "{snippet} {snippet}" "snippet" "snip")
        "snip snip" );
  ]

let make_fetch_test (name : string) (command : command) (result : string list) =
  name >:: fun _ -> assert_equal (execute command) result

let fetch_tests =
  [
    make_fetch_test "fetch name test"
      (Fetch (Name, [ "Sanders"; "Bernard" ]))
      [ "Bernard Sanders" ];
    make_fetch_test "fetch party test"
      (Fetch (Party, [ "Booker"; "Cory A." ]))
      [ "D" ];
    make_fetch_test "fetch state test"
      (Fetch (State, [ "Sanders"; "Bernard" ]))
      [ "VT" ];
    make_fetch_test "fetch address test"
      (Fetch (Address, [ "Schumer"; "Charles E." ]))
      [ "322 Hart Senate Office Building Washington DC 20510" ];
    make_fetch_test "fetch phone test"
      (Fetch (Phone, [ "Baldwin"; "Tammy" ]))
      [ "(202) 224-5653" ];
    make_fetch_test "fetch address test"
      (Fetch (Email, [ "Padilla"; "Alex" ]))
      [ "https://www.padilla.senate.gov/contact/" ];
    make_fetch_test "fetch class test"
      (Fetch (Class, [ "Warren"; "Elizabeth" ]))
      [ "Class I" ];
    make_fetch_test "fetch committees test"
      (Fetch (Committees, [ "Sanders"; "Bernard" ]))
      [
        "Committee on Energy and Natural Resources";
        "Committee on Environment and Public Works";
        "Committee on Health, Education, Labor, and Pensions";
        "Committee on the Budget";
        "Committee on Veterans' Affairs";
      ];
  ]

let tests =
  "test suite for project"
  >::: List.flatten [ scraper_tests; parser_tests; markdown_tests; fetch_tests ]

let _ = run_test_tt_main tests
