open Gtk
open Senate
open Command
open Pango

(* Constants to avoid use of magic numbers*)
let width_of_window = 1000
let height_of_window = 675
let x_coordinate_of_button = 10
let y_coordinate_of_button = 10
let max_length_of_text = 10
let width_of_button = 20
let height_of_button = 30
let width_of_entry = 80
let height_of_entry = 40

let create_popup_dialog () =
  let dialog =
    GWindow.message_dialog
      ~message:"An unexpected error occurred while web scraping."
      ~buttons:GWindow.Buttons.close ~message_type:`ERROR ()
  in
  dialog#set_position `CENTER_ALWAYS;

  ignore (dialog#run ());
  dialog#destroy ()

(* Takes at most 5 elements from a list *)
let take lst =
  match lst with
  | a :: b :: c :: d :: e :: _ -> [ a; b; c; d; e ]
  | _ -> lst (* Return the original list if it has less than 5 elements *)

(* Button with constraints on size *)
let create_button fixed_container =
  let button = GButton.button ~label:"Test press" () in
  ignore (button#connect#clicked ~callback:(fun () -> print_endline "Sample"));
  fixed_container#put button#coerce ~x:x_coordinate_of_button
    ~y:y_coordinate_of_button;
  button

(* Similar function from bin/main.ml that handles the input from the text entry
   but with major changes to handling certain cases*)
let handle cmd =
  let open Command in
  match cmd |> parse with
  | exception Empty -> "Please write in an argument!"
  | exception Invalid -> "Invalid argument"
  | Quit ->
      GMain.Main.quit ();
      ""
  | List -> Executor.execute List |> String.concat "\t"
  | Fetch (f, o) -> (
      let open Executor in
      try execute (Fetch (f, o)) |> String.concat "\n" with
      | BadArgument -> "Invalid Senator"
      | UnexpectedError ->
          create_popup_dialog ();
          "")
  (* Need to handle Export case *)
  | _ -> ""

(* Initial window with Welcome label and How-to label. Also with button to
   execute webscraping and a text entry field. Results are shown on a label
   below the text entry field. *)
let create_window () =
  let window =
    GWindow.window ~title:"Senate Stalker GUI" ~width:width_of_window
      ~height:height_of_window ()
  in
  ignore (window#connect#destroy ~callback:(fun () -> GMain.Main.quit ()));

  window#set_position `CENTER;
  window#set_resizable true;
  window#set_modal true;

  let vbox = GPack.vbox ~packing:window#add () in

  let firstlabel =
    GMisc.label ~text:"Senate Stalker: Learn more about our Senators!"
      ~packing:vbox#add ()
  in

  firstlabel#misc#modify_font_by_name "Georgia 24";

  let blue_color = GDraw.color (`NAME "blue") in
  firstlabel#misc#modify_fg [ (`NORMAL, `COLOR blue_color) ];

  let secondlabel =
    GMisc.label
      ~markup:
        "To get a list of senators, please enter the command <b>List</b>\n\
         To get information on a specific senator, please enter <b>Fetch</b> \
         [<i>field</i>] [<i>full name of senator</i>] \n\
         Supported values of [<i>field</i>] are: \n\
        \    Name\n\
        \    Party\n\
        \    State\n\
        \    Address\n\
        \    Phone\n\
        \    Email\n\
        \    Website\n\
        \    Class\n\
        \    Committees\n"
      ~packing:vbox#add ()
  in

  let fuchsia_color = GDraw.color (`NAME "fuchsia") in
  secondlabel#misc#modify_fg [ (`NORMAL, `COLOR fuchsia_color) ];

  secondlabel#misc#modify_font_by_name "Serif 20";

  let hbox = GPack.hbox ~spacing:6 ~packing:vbox#add () in

  let entry =
    GEdit.entry ~width:width_of_entry ~height:height_of_entry ~packing:hbox#add
      ()
  in

  entry#misc#modify_font_by_name "Times New Roman 22";

  (* Capitalize first letter of input *)
  ignore
    (entry#connect#changed ~callback:(fun () ->
         let text = entry#text in
         match String.length text with
         | 0 -> () (* Empty entry, nothing to capitalize *)
         | _ ->
             let capitalized_text = String.capitalize_ascii text in
             entry#set_text capitalized_text));

  let button = GButton.button ~packing:hbox#add () in
  button#misc#set_size_request ~width:width_of_button ~height:height_of_button
    ();

  (* Function to allow enter key activate button press *)
  ignore (entry#connect#activate ~callback:(fun () -> button#clicked ()));

  let font_desc = Pango.Font.from_string "Georgia 24" in
  let label = GMisc.label ~markup:"Stalk a <i>Senator</i>!" () in
  label#misc#modify_font font_desc;
  button#add label#coerce;

  let orange_color = GDraw.color (`NAME "orange") in
  button#misc#modify_bg [ (`NORMAL, `COLOR orange_color) ];

  let red_color = GDraw.color (`NAME "red") in
  ignore
    (button#event#connect#enter_notify ~callback:(fun _ ->
         button#misc#modify_bg [ (`NORMAL, `COLOR red_color) ];
         true));

  let orange_color = GDraw.color (`NAME "orange") in
  ignore
    (button#event#connect#leave_notify ~callback:(fun _ ->
         button#misc#modify_bg [ (`NORMAL, `COLOR orange_color) ];
         true));

  let result_label = GMisc.label ~text:"" ~packing:vbox#add () in
  ignore
    (button#connect#clicked ~callback:(fun () ->
         let text = entry#text in
         result_label#set_text (handle text)));

  result_label#misc#modify_font_by_name "Serif 20";
  result_label#set_line_wrap true;
  result_label#set_selectable true;

  result_label#set_max_width_chars 1000;

  result_label#set_justify `LEFT;
  result_label#misc#set_size_request ~width:200 ~height:100 ();

  let history_label = GMisc.label ~packing:hbox#add () in

  history_label#misc#modify_font_by_name "Palatino 16";
  history_label#misc#modify_bg
    [ (`NORMAL, `COLOR (GDraw.color (`NAME "gray"))) ];

  let history_list = ref [] in

  ignore
    (button#connect#clicked ~callback:(fun () ->
         (* Get the current text from the entry widget *)
         let text = result_label#text in

         if text != "Invalid argument" || text != "Invalid Senator" then
           (* Add the text to the history list *)
           history_list := text :: !history_list;

         (* Limit the history list to the last 5 entries *)
         history_list := take !history_list;

         (* Update the history label *)
         history_label#set_text (String.concat "\n" (List.rev !history_list));

         (* Clear the entry widget *)
         entry#set_text ""));

  window#show ();
  (entry, button, result_label)

(* Build up window *)
let main () =
  ignore (GtkMain.Main.init ());
  let _, _, _ = create_window () in
  GtkMain.Main.main ()

let () = main ()
