open OUnit2
open Senate
open Scraper
open Command
open Executor
open Csv_parser

(* Manual testing was done primarily for our GUI. All 4 members extensively
   checked and manually tested the GUI features and functionality. This also
   includes the terminal-based UI, where all members again, tested and checked
   through all commands. The following tests verify the functions used in the
   GUI and Terminal-based UI are correct. *)

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
    ( "Committees test for simple first/last name" >:: fun _ ->
      assert_equal
        (Scraper.Committees.exec "Welch, Peter")
        [
          "Committee on Agriculture, Nutrition, and Forestry";
          "Committee on Commerce, Science, and Transportation";
          "Committee on Rules and Administration";
          "Committee on the Judiciary";
          "Joint Economic Committee";
        ] );
    ( "committtees test for invalid senator" >:: fun _ ->
      assert_raises UnknownSenator (fun () ->
          Scraper.Committees.exec "not a senator") );
    ( "check list of senator length" >:: fun _ ->
      assert_equal 100 (Scraper.Members.exec () |> List.length) );
  ]

let make_parse_test (name : string) (command : string) (result : command) =
  name >:: fun _ -> assert_equal (parse command) result

let parse_tests =
  [
    make_parse_test "parse quit" "Quit" Quit;
    make_parse_test "parse list" "List" List;
    make_parse_test "parse fetch name" "Fetch Name test"
      (Fetch (Name, [ "test" ]));
    make_parse_test "parse fetch party" "Fetch Party test"
      (Fetch (Party, [ "test" ]));
    make_parse_test "parse fetch state" "Fetch State test"
      (Fetch (State, [ "test" ]));
    make_parse_test "parse fetch address" "Fetch Address test"
      (Fetch (Address, [ "test" ]));
    make_parse_test "parse fetch phone" "Fetch Phone test"
      (Fetch (Phone, [ "test" ]));
    make_parse_test "parse fetch email" "Fetch Email test"
      (Fetch (Email, [ "test" ]));
    make_parse_test "parse fetch website" "Fetch Website test"
      (Fetch (Website, [ "test" ]));
    make_parse_test "parse fetch class" "Fetch Class test"
      (Fetch (Class, [ "test" ]));
    make_parse_test "parse fetch committees" "Fetch Committees test"
      (Fetch (Committees, [ "test" ]));
    make_parse_test "parse fetch nominate" "Fetch Nominate test"
      (Fetch (DWNom, [ "test" ]));
    make_parse_test "parse finance nominate" "Fetch Finance test"
      (Fetch (Finance, [ "test" ]));
    make_parse_test "two item arg phrase" "Fetch Name a b"
      (Fetch (Name, [ "b"; "a" ]));
    make_parse_test "two item arg phrase with extra white space"
      "Fetch Name a    b"
      (Fetch (Name, [ "b"; "a" ]));
    make_parse_test "three item arg phrase" "Fetch Name a b c"
      (Fetch (Name, [ "c"; "b"; "a" ]));
    make_parse_test "parse empty arg phrase" "Fetch Name" (Fetch (Name, []));
    ( "parse empty string (should raise Empty)" >:: fun _ ->
      assert_raises Empty (fun () -> parse "") );
    ( "parse empty string with whitespace" >:: fun _ ->
      assert_raises Empty (fun () -> parse "    ") );
    ( "parse bad command: quit with arguments" >:: fun _ ->
      assert_raises Invalid (fun () -> parse "quit bad args") );
    ( "parse bad command: list with arguments" >:: fun _ ->
      assert_raises Invalid (fun () -> parse "list bad args") );
    make_parse_test "parse command export: typical case"
      "Export data/test Bernard Sanders"
      (Export ("data/test", [ "Sanders"; "Bernard" ]));
    make_parse_test "parse command export with empty arg phrase"
      "Export data/test"
      (Export ("data/test", []));
  ]

let markdown_tests =
  let d = [ ("name", "Gorg Abbott"); ("word", "ipsum"); ("snippet", "snip") ] in
  let open Markdown in
  [
    ( "basic snippet replacement tests" >:: fun _ ->
      assert_equal (replace_snippet "{name}" d) "Gorg Abbott" );
    ( "embedded snippet test" >:: fun _ ->
      assert_equal
        (replace_snippet "Lorem {word} dolor sit amet" d)
        "Lorem ipsum dolor sit amet" );
    ( "multiple identical tokens" >:: fun _ ->
      assert_equal (replace_snippet "{snippet} {snippet}" d) "snip snip" );
  ]

let make_fetch_test (name : string) (command : command) (result : string list) =
  name >:: fun _ -> assert_equal (execute command) result

let fetch_tests =
  [
    make_fetch_test "fetch name test"
      (Fetch (Name, [ "Sanders"; "Bernard" ]))
      [ "Sanders, Bernard" ];
    make_fetch_test "fetch party test"
      (Fetch (Party, [ "Sanders"; "Bernard" ]))
      [ "I" ];
    make_fetch_test "fetch state test"
      (Fetch (State, [ "Sanders"; "Bernard" ]))
      [ "VT" ];
    make_fetch_test "fetch address test"
      (Fetch (Address, [ "Sanders"; "Bernard" ]))
      [ "332 Dirksen Senate Office Building Washington DC 20510" ];
    make_fetch_test "fetch phone test"
      (Fetch (Phone, [ "Sanders"; "Bernard" ]))
      [ "(202) 224-5141" ];
    make_fetch_test "fetch address test"
      (Fetch (Email, [ "Sanders"; "Bernard" ]))
      [ "https://www.sanders.senate.gov/contact/" ];
    make_fetch_test "fetch class test"
      (Fetch (Class, [ "Sanders"; "Bernard" ]))
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
    make_fetch_test "fetch nominate test"
      (Fetch (DWNom, [ "Sanders"; "Bernard" ]))
      [
        "-0.538";
        "The score is on a scale from -1.0 to 1.0, where -1 is the most \
         liberal possible score and 1 is the most conservative. The Democratic \
         Party median is -0.3455 and the Republican median is 0.541.";
      ];
    make_fetch_test "fetch finance test"
      (Fetch (Finance, [ "Sanders"; "Bernard" ]))
      [
        "Total Campaign Receipts: $28,938,549.60";
        "Total Contributions Received: $26,692,520.35";
        "Total Individual Contributions Received: $26,687,520.35";
      ];
  ]

let make_row_test name r header expected =
  name >:: fun _ -> assert_equal (row r header) expected

let csv_tests =
  let h1 = header_row "a,b,c" in
  [
    ("blank header" >:: fun _ -> assert_equal (header_row "") [ "" ]);
    ("1 item header" >:: fun _ -> assert_equal (header_row "a") [ "a" ]);
    ("2 item header" >:: fun _ -> assert_equal (header_row "a,b") [ "a"; "b" ]);
    ("3 item header" >:: fun _ -> assert_equal h1 [ "a"; "b"; "c" ]);
    make_row_test "typical row parsing case" "1,2,3" h1
      [ ("a", "1"); ("b", "2"); ("c", "3") ];
    make_row_test "escape a comma with double quotes" "\"1,2\",3,4" h1
      [ ("a", "\"1,2\""); ("b", "3"); ("c", "4") ];
    ( "Bad header (should raise exception)" >:: fun _ ->
      assert_raises (Invalid_argument "Csv_parser.row") (fun () -> row "1,2" [])
    );
  ]

let tests =
  "test suite for project"
  >::: List.flatten
         [ scraper_tests; parse_tests; markdown_tests; fetch_tests; csv_tests ]

let _ = run_test_tt_main tests
