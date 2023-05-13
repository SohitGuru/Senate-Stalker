open Gtk
open GMain
open GdkKeysyms
open GdkPixbuf
open Pango
open Senate
open Command

(* Constants to avoid use of magic numbers*)
let width_of_window = 1000
let height_of_window = 750
let x_coordinate_of_button = 10
let y_coordinate_of_button = 10
let max_length_of_text = 10
let width_of_button = 15
let height_of_button = 30
let width_of_entry = 150
let height_of_entry = 40
let scuffedwidth_of_resultlabel = 975
let scuffedheight_of_resultlabel = 225
let normalwidth_of_resultlabel = 400
let normalheight_of_resultlabel = 200
let spacing_between_boxes = 30
let previous_texts = ref []
let current_index = ref (-1)

(* Form of history; if you press the up key, you get your previous text entry
   you wrote. *)
let on_key_press entry event : bool =
  let key = GdkEvent.Key.keyval event in
  match key with
  | 65362 (* GdkKeysyms._Up *) ->
      let num_entries = List.length !previous_texts in
      if num_entries > 0 then (
        current_index := (!current_index - 1 + num_entries) mod num_entries;
        entry#set_text (List.nth !previous_texts !current_index));
      true
  | 65364 ->
      let num_entries = List.length !previous_texts in
      if num_entries > 0 then
        if !current_index = num_entries - 1 then entry#set_text ""
        else (
          current_index := (!current_index + 1) mod num_entries;
          entry#set_text (List.nth !previous_texts !current_index));
      true
  | _ -> false

(* Creates an about popup that explains *)
let create_aboutpopup_dialog () =
  let dialog =
    GWindow.message_dialog
      ~message:
        "Senate Stalker is a program created by the CS3110 project team: \
         Keaton-Camels. The team consists of Anya Khanna (netid: ak2243), \
         Dayana Soria Lopez (netid: dns64), Jordan Rudolph (netid: jmr489), \
         and Sohit Gurung (netid: sg857). Senate Stalker provides information \
         on a senator specified by the client such as Party, State, Address, \
         Phone Number, Email, Website, and Committees the senator is part of. \
         Users also have the option to download this information as well, in \
         order to better understand our current senators."
      ~buttons:GWindow.Buttons.ok ~message_type:`INFO ()
  in
  dialog#set_position `CENTER_ALWAYS;

  ignore (dialog#run ());
  dialog#destroy ()

(* Creates error message if web scraping encounters error. *)
let create_popup_dialog () =
  let dialog =
    GWindow.message_dialog
      ~message:"An unexpected error occurred while web scraping."
      ~buttons:GWindow.Buttons.close ~message_type:`ERROR ()
  in
  dialog#set_position `CENTER_ALWAYS;

  ignore (dialog#run ());
  dialog#destroy ()

(* Creates a help pop-up *)
let create_helppopup_dialog () =
  let dialog =
    GWindow.message_dialog
      ~message:
        "To get started, simply write either List to get a list of all the \
         names of the senators or Fetch followed by a name of a senator and a \
         field specifying what it is you want to know about the senator. \
         After, if you want to save the information you found in a .md file, \
         simply write Export followed by the pathname and name of senator. "
      ~buttons:GWindow.Buttons.ok ~message_type:`INFO ()
  in
  dialog#set_position `CENTER_ALWAYS;

  ignore (dialog#run ());
  dialog#destroy ()

(* Takes at most 5 elements from a list *)
let take lst =
  match lst with
  | a :: b :: c :: d :: e :: _ -> [ a; b; c; d; e ]
  | _ -> lst (* Return the original list if it has less than 5 elements *)

(* Similar function from bin/main.ml that handles the input from the text entry
   but with major changes to handling certain cases. *)
let handle cmd =
  if String.contains ""
  let open Command in
  match cmd |> parse with
  | exception Empty -> "Please write in an argument!"
  | exception Invalid -> "Invalid argument"
  | Quit ->
      GMain.Main.quit ();
      ""
  | List -> Executor.execute List |> String.concat ", "
  | Fetch (f, o) -> (
      let open Executor in
      try
        execute (Fetch (f, o))
        |> String.concat (if f = Committees then ", " else "\n")
      with
      | BadArgument -> "Invalid Senator"
      | UnexpectedError ->
          create_popup_dialog ();
          "")
  (* Need to handle Export case *)
  | Export (p, s) -> (
      try String.concat "\n" (Executor.execute (Export (p, s)))
      with _ -> "An error occured")

(* Checks if the command is Fetch Committees or not. *)
let check_string str =
  let prefix = "Fetch Committees" in
  let len_prefix = String.length prefix in
  if String.length str >= len_prefix && String.sub str 0 len_prefix = prefix
  then true
  else false

(* Scales image according to dimensions specified. *)
let scale_pixbuf pixbuf width height =
  let new_pixbuf = GdkPixbuf.create ~width ~height ~has_alpha:true () in
  GdkPixbuf.scale ~dest:new_pixbuf ~width ~height ~interp:`BILINEAR pixbuf;
  new_pixbuf

let create_export_popup () =
  let selected w () =
    match w#filename with
    | Some s ->
        (* Executor.execute (Export (s ^ "/senstalk.md", [ "Sanders"; "Bernard" ])) *)
        print_endline s; (* TODO change this*)
        |> ignore
    | None -> ()
  in
  let widget = GWindow.file_chooser_dialog `SELECT_FOLDER () in
  (* let widget = GWindow.file_selection ~title:"File selection"
     ~border_width:10 () in *)
  print_endline "Export";
  widget#add_select_button_stock `OK `OPEN;
  widget#connect#current_folder_changed (selected widget) |> ignore;
  (* ~callback:(selected widget) *)
  (* ~callback:(selected widget) *)
  widget#show ();
  ignore (widget#run ());
  widget#destroy ()

(* Initial window with Welcome label and How-to label. Also with button to
   execute webscraping and a text entry field. Results are shown on a label
   below the text entry field. *)
let create_window () =
  let icon_pixbuf =
    scale_pixbuf (GdkPixbuf.from_file "data/senatestalkerlogo.ico") 32 32
  in

  let window =
    GWindow.window ~title:"Senate Stalker GUI" ~width:width_of_window
      ~icon:icon_pixbuf ~height:height_of_window ()
  in
  ignore (window#connect#destroy ~callback:(fun () -> GMain.Main.quit ()));

  window#set_position `CENTER;
  window#set_resizable true;
  window#set_modal true;

  let vbox = GPack.vbox ~spacing:spacing_between_boxes ~packing:window#add () in

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#add () in

  let menu =
    GMenu.menu
      ~packing:
        (GMenu.menu_item ~label:"Menu" ~packing:menubar#append ())#set_submenu
      ()
  in
  GToolbox.build_menu menu
    ~entries:
      [
        `I ("About", fun () -> create_aboutpopup_dialog ());
        `I ("History", fun () -> print_endline "history");
        `I ("Help", fun () -> create_helppopup_dialog ());
        `I ("Export", fun () -> create_export_popup ());
        `S;
        `I ("Quit", GMain.Main.quit);
      ];

  menu#set_border_width 1;

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
        \    Committees\n\
        \    Export\n"
      ~packing:vbox#add ()
  in

  let fuchsia_color = GDraw.color (`NAME "fuchsia") in
  secondlabel#misc#modify_fg [ (`NORMAL, `COLOR fuchsia_color) ];

  secondlabel#misc#modify_font_by_name "Serif 20";

  let hbox = GPack.hbox ~spacing:spacing_between_boxes ~packing:vbox#add () in

  let entry =
    GEdit.entry ~width:width_of_entry ~height:height_of_entry ~packing:hbox#add
      ()
  in

  entry#misc#modify_font_by_name "Times New Roman 22";

  entry#event#add [ `KEY_PRESS ];
  ignore (entry#event#connect#key_press ~callback:(on_key_press entry));

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

  let font_desc = Pango.Font.from_string "Georgia 24" in
  let label = GMisc.label ~markup:"Stalk a <i>Senator</i>!" () in
  label#misc#modify_font font_desc;
  button#add label#coerce;

  let result_label =
    GMisc.label ~text:"" ~height:normalheight_of_resultlabel
      ~width:normalwidth_of_resultlabel ~packing:vbox#add ()
  in

  result_label#misc#modify_font_by_name "Serif 24";
  result_label#set_line_wrap true;
  result_label#set_selectable true;
  result_label#set_justify `CENTER;

  let button_callback : unit -> unit =
   fun () ->
    let text1 = entry#text in
    previous_texts := List.append !previous_texts [ text1 ];
    result_label#set_text
      (if text1 = "List" || check_string text1 then (
       result_label#misc#modify_font_by_name "Serif 16.5";
       result_label#misc#set_size_request ~width:scuffedwidth_of_resultlabel
         ~height:scuffedheight_of_resultlabel ();
       handle text1)
      else (
        result_label#misc#modify_font_by_name "Serif 24";
        result_label#misc#set_size_request ~width:normalwidth_of_resultlabel
          ~height:normalheight_of_resultlabel ();
        handle text1));

    (* Clear the entry widget *)
    entry#set_text ""
  in

  (* Connect the button click event to the callback function *)
  ignore (button#connect#clicked ~callback:(fun () -> button_callback ()));

  (* Function to allow enter key activate button press *)
  ignore (entry#connect#activate ~callback:(fun () -> button#clicked ()));

  let orange_color = GDraw.color (`NAME "orange") in
  button#misc#modify_bg [ (`NORMAL, `COLOR orange_color) ];

  let red_color = GDraw.color (`NAME "red") in
  ignore
    (button#connect#enter ~callback:(fun () ->
         button#misc#modify_bg [ (`ACTIVE, `COLOR red_color) ]));
  ignore
    (button#connect#enter ~callback:(fun () ->
         button#misc#modify_bg [ (`PRELIGHT, `COLOR red_color) ]));

  let orange_color = GDraw.color (`NAME "orange") in
  ignore
    (button#connect#leave ~callback:(fun () ->
         button#misc#modify_bg [ (`NORMAL, `COLOR orange_color) ]));

  window#show ();
  (entry, button, result_label)

(* Build up window *)
let main () =
  ignore (GtkMain.Main.init ());
  let _, _, _ = create_window () in
  GtkMain.Main.main ()

let () = main ()
