open OUnit2
open Senate
open Scraper
open Command
open Executor
open Csv_parser
(*****************************************************************)

(* TEST PLAN

   Manual testing was done primarily for our GUI. All 4 members extensively
   checked and manually tested the GUI features and functionality. This also
   includes the terminal-based UI, where all members again, tested and checked
   through all commands. The following tests verify the functions used in the
   GUI and Terminal-based UI are correct.

   For some of our datasets, there were issues between the scraping and OUnit.
   Since web scraping can involve a lot of redirects and is unreliable with its
   speed, the more complex issues were causing issues with hanging in OUnit. As
   such, these datasets were left out of our fetch tests below. Instead, they
   were manually and thoroughly tested using utop, the repl, and the gui.

   Additionally, the export feature was not automatically tested since it
   requires the creation of the file. It was thoroughly manually tested with
   utop, the repl interface, and the gui interface. *)

(*****************************************************************)

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
    ( "read/writing from test data" >:: fun _ ->
      let st = Unix.time () |> string_of_float in
      assert_equal
        (write_str "data/test" st;
         string_of_file "data/test")
        st );
    ( "Reading a nonexistent file" >:: fun _ ->
      assert_raises (Sys_error "data/nonexistent: No such file or directory")
        (fun () -> string_of_file "data/nonexistent") );
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
  ]

let map_tests =
  let open Map in
  let m = make (fun x -> x) 2 in
  [
    ("Empty map size" >:: fun _ -> assert_equal (size m) 0);
    ( "insert test" >:: fun _ ->
      assert_equal
        (put 1 15 m;
         get 1 m)
        15 );
    ( "Insert overrwrite test" >:: fun _ ->
      assert_equal
        (put 1 16 m;
         get 1 m)
        16 );
    ("Check size after overrwrite" >:: fun _ -> assert_equal (size m) 1);
    ( "Check size after resize (glass box)" >:: fun _ ->
      assert_equal
        (put 2 17 m;
         put 3 18 m;
         put 4 19 m;
         put 5 20 m;
         size m)
        5 );
    ( "Check bindings after resize (glass box)" >:: fun _ ->
      assert_bool "checking all bindings"
        (get 1 m = 16
        && get 2 m = 17
        && get 3 m = 18
        && get 4 m = 19
        && get 5 m = 20) );
    ("Bad get" >:: fun _ -> assert_raises Not_found (fun () -> get 131940 m));
    ( "Bad make" >:: fun _ ->
      assert_raises (Invalid_argument "Map.make") (fun () ->
          make (fun x -> x) ~-2) );
    ( "remove test" >:: fun _ ->
      assert_raises Not_found (fun () ->
          remove 2 m;
          get 2 m) );
    ( "bad remove" >:: fun _ ->
      assert_equal (size m)
        (remove 9123 m;
         size m) );
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
    ( "Nominate typical case" >:: fun _ ->
      assert_equal (DWNominate.exec "SANDERS, Bernard") ~-.0.538 );
    ( "Nominate nonexistent senator" >:: fun _ ->
      assert_raises UnknownSenator (fun () ->
          DWNominate.exec "nonexistent senator") );
  ]

let type_tests =
  (* glass box tests for (the relatively simple) compilation units that
     primarily exist to represent different abstract types *)
  [
    ( "Member type test" >:: fun _ ->
      let open Member in
      let m = make ("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k") in
      assert_bool "checking all member fields"
        (full_name m = "a"
        && last_name m = "b"
        && first_name m = "c"
        && party m = "d"
        && state m = "e"
        && address m = "f"
        && phone m = "g"
        && email m = "h"
        && website m = "i"
        && class_num m = "j"
        && biographical_id m = "k") );
    ( "Finance type test" >:: fun _ ->
      let open Finance in
      let m = make ("a", "b", "c") in
      assert_bool "checking all finance fields"
        (receipts m = "a"
        && total_contributions m = "b"
        && indiv_contributions m = "c") );
    ( "Stockinfo type test" >:: fun _ ->
      let open Stockinfo in
      let m = make ("a", "b", "c", "d") in
      assert_bool "checking all stockinfo fields"
        (company m = "a"
        && transaction_type m = "b"
        && amount m = "c"
        && trade_date m = "d") );
  ]

let tests =
  "test suite for project"
  >::: List.flatten
         [
           scraper_tests;
           parse_tests;
           markdown_tests;
           fetch_tests;
           csv_tests;
           map_tests;
           type_tests;
         ]

let _ = run_test_tt_main tests
