open Gtk
open GMain
open GdkKeysyms
open GDraw
open GdkPixbuf
open Pango
open Senate
open Command

(* Constants to avoid use of magic numbers*)
let width_of_window = 1100
let height_of_window = 765
let border_width_of_window = 2
let width_of_button = 15
let height_of_button = 30
let width_of_entry = 150
let height_of_entry = 50
let normalwidth_of_resultlabel = 600
let normalheight_of_resultlabel = 200
let spacing_between_boxes = 10
let spacing_between_entry_and_button = 25
let previous_texts = ref []
let current_index = ref (-1)
let width_of_historydialog = 300
let height_of_historydialog = 250
let width_of_erasedialog = 350
let height_of_erasedialog = 75
let width_of_senatordialog = 350
let height_of_senatordialog = 315

(* Form of history; if you press the up key, you get your previous text entries
   you wrote. Press down to view more recent previous entries. *)
let on_key_press entry event : bool =
  let key = GdkEvent.Key.keyval event in
  match key with
  | 65362 (* GdkKeysyms._Up *) ->
      let num_entries = List.length !previous_texts in
      if num_entries > 0 then (
        current_index := (!current_index - 1 + num_entries) mod num_entries;
        entry#set_text (List.nth !previous_texts !current_index));
      true
  | 65364 (* GdkKeysyms._Down *) ->
      let num_entries = List.length !previous_texts in
      if num_entries > 0 then
        if !current_index = num_entries - 1 then entry#set_text ""
        else (
          current_index := (!current_index + 1) mod num_entries;
          entry#set_text (List.nth !previous_texts !current_index));
      true
  | _ -> false

(* Creates an about popup introducing our team and oriject *)
let create_aboutpopup_dialog () =
  let dialog =
    GWindow.about_dialog ~name:"Senate Stalker" ~title:"About Us"
      ~destroy_with_parent:true
      ~authors:
        [
          "Anya Khanna (netid: ak2243)";
          "Dayana Soria Lopez (netid: dns64)";
          "Jordan Rudolph (netid: jmr489)";
          "Sohit Gurung (netid: sg857)";
        ]
      ~comments:
        "Senate Stalker is a program created by the CS3110 project team: \
         Keaton-Camels. Senate Stalker provides information on a senator \
         specified by the client such as Party, State, Address, Phone Number, \
         Email, Website, and Committees the senator is part of. Users also \
         have the option to download this information as well, in order to \
         better understand our current senators."
      ~decorated:true
      ~logo:(GdkPixbuf.from_file "data/senatestalkerlogo.ico")
      ()
  in
  dialog#set_position `CENTER_ALWAYS;

  ignore (dialog#run ());
  dialog#destroy ()

(* Creates error message if web scraping encounters error. *)
let create_popup_dialog () =
  let dialog =
    GWindow.message_dialog
      ~message:"An unexpected error occurred while web scraping."
      ~buttons:GWindow.Buttons.close ~message_type:`ERROR
      ~destroy_with_parent:true ()
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
      ~buttons:GWindow.Buttons.ok ~message_type:`INFO ~destroy_with_parent:true
      ()
  in
  dialog#set_position `CENTER_ALWAYS;

  ignore (dialog#run ());
  dialog#destroy ()

(* Takes at most, the last 5 elements from a list *)
let take_last_five lst =
  let rec aux acc remaining =
    match remaining with
    | [] -> List.rev acc
    | _ :: tail ->
        if List.length acc < 5 then aux (List.hd remaining :: acc) tail
        else aux (List.tl acc @ [ List.hd remaining ]) tail
  in
  aux [] lst

(* Function that converts a list into a bulleted list. *)
let to_bulleted_string lst =
  match lst with
  | [] -> "Your history is empty 💤"
  | _ ->
      let bullet_point = "• " in
      let lines = List.map (fun x -> bullet_point ^ x) lst in
      String.concat "\n" lines

(* Erases all previous search entries. *)
let erase_history () : bool =
  if List.length !previous_texts <> 0 then (
    previous_texts := [];
    true)
  else false

(* Erase history pop-up. *)
let create_erasehistory_dialog () =
  let dialog =
    GWindow.dialog ~position:`CENTER_ALWAYS ~width:width_of_erasedialog
      ~height:height_of_erasedialog ~destroy_with_parent:true ()
  in
  let label = GMisc.label () in
  if erase_history () then begin
    label#set_text "Your history has been erased.";
    dialog#set_title "Cleared"
  end
  else begin
    label#set_text "Your history is empty, enter some commands to get started.";
    dialog#set_title "Empty History"
  end;
  ignore (dialog#vbox#add label#coerce);
  ignore (dialog#add_button_stock `CLOSE `CLOSE);

  dialog#show ();
  ignore (dialog#run ());
  dialog#destroy ()

(* Displays your search history. *)
let create_history_dialog () =
  let dialog =
    GWindow.dialog ~title:"History" ~width:width_of_historydialog
      ~height:height_of_historydialog
      ~icon:(GdkPixbuf.from_file "data/stalk.png")
      ~destroy_with_parent:true ~position:`CENTER_ALWAYS ()
  in
  let label =
    GMisc.label ~text:(take_last_five !previous_texts |> to_bulleted_string) ()
  in
  let _ = dialog#vbox#add label#coerce in
  let _ = dialog#add_button_stock `CLOSE `CLOSE in

  dialog#show ();
  ignore (dialog#run ());
  dialog#destroy ()

(* Similar function from bin/main.ml that handles the input from the text entry
   but with major changes to handling certain cases. *)
let handle cmd =
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
      try execute (Fetch (f, o)) |> String.concat "\n" with
      | BadArgument -> "Invalid Senator"
      | UnexpectedError ->
          create_popup_dialog ();
          "")
  | Export (p, s) -> (
      try String.concat "\n" (Executor.execute (Export (p, s)))
      with _ -> "An error occured")

(* Returns a string for today's date in MM/DD/YYYY *)
let todays_date =
  let now = Unix.localtime (Unix.time ()) in
  let year = now.Unix.tm_year + 1900 in
  let month = now.Unix.tm_mon + 1 in
  let day = now.Unix.tm_mday in
  Printf.sprintf "Today's Date:  %d/%d/%d" month day year

(* Creates a pop-up on Senate Elections *)
let create_senateelectionpopup_dialog () =
  let dialog =
    GWindow.message_dialog
      ~message:
        "United States senator elections are held every two years. During \
         these elections, approximately one-third of the Senate seats are up \
         for grabs. This means that out of the 100 total Senate seats, around \
         33 seats are contested in each election cycle. These elections are \
         usually held in November; the next election is set to be held on \
         November 5, 2024 (11/05/2024)."
      ~buttons:GWindow.Buttons.close ~message_type:`INFO
      ~destroy_with_parent:true ()
  in
  dialog#set_position `CENTER_ALWAYS;

  ignore (dialog#run ());
  dialog#destroy ()

let classonedate = [ `I ("1/3/2025", fun () -> ()) ]
let classtwodate = [ `I ("1/3/2027", fun () -> ()) ]
let classthreedate = [ `I ("1/3/2029", fun () -> ()) ]

(* Checks if the command is Fetch Committees or not. *)
let check_string str =
  let prefix = "Fetch Committees" in
  let len_prefix = String.length prefix in
  let otherprefix = "Fetch Stocks" in
  let len_otherprefix = String.length otherprefix in
  if
    (String.length str >= len_prefix && String.sub str 0 len_prefix = prefix)
    || String.length str >= len_otherprefix
       && String.sub str 0 len_otherprefix = otherprefix
  then true
  else false

let button_callback (entry : GEdit.entry) (result_label : GMisc.label)
    (scrolled_window : GBin.scrolled_window) () =
  let text1 = entry#text in
  if text1 <> "" && text1 <> "Clear" then current_index := !current_index + 1;
  previous_texts := List.append !previous_texts [ text1 ];
  result_label#set_text
    (if text1 = "List" || check_string text1 then (
     result_label#misc#modify_font_by_name "Serif 22";
     handle text1)
    else if text1 = "Clear" (* Clear all search results and entry text *) then (
      result_label#misc#modify_font_by_name "Serif 24";
      scrolled_window#misc#set_size_request ~width:normalwidth_of_resultlabel
        ~height:normalheight_of_resultlabel ();
      result_label#misc#set_size_request ~width:normalwidth_of_resultlabel
        ~height:normalheight_of_resultlabel ();
      "")
    else (
      result_label#misc#modify_font_by_name "Serif 24";
      result_label#misc#set_size_request ~width:normalwidth_of_resultlabel
        ~height:normalheight_of_resultlabel ();
      handle text1));
  if
    result_label#text <> "Invalid Senator"
    && result_label#text <> "Invalid argument"
  then (* Clear the entry widget *)
    entry#set_text ""

(* Capitalize the beginning of a string. *)
let capitalize entry =
  let text = entry#text in
  match String.length text with
  | 0 -> () (* Empty entry, nothing to capitalize *)
  | _ ->
      let capitalized_text = String.capitalize_ascii text in
      entry#set_text capitalized_text

(* Scales image according to dimensions specified. *)
let scale_pixbuf pixbuf width height =
  let new_pixbuf = GdkPixbuf.create ~width ~height ~has_alpha:true () in
  GdkPixbuf.scale ~dest:new_pixbuf ~width ~height ~interp:`BILINEAR pixbuf;
  new_pixbuf

let display_popup s =
  (* create a popup with the text s *)
  let dialog =
    GWindow.message_dialog ~message:s ~buttons:GWindow.Buttons.ok
      ~message_type:`INFO ~destroy_with_parent:true ~modal:true ()
  in
  dialog#set_position `CENTER_ALWAYS;

  ignore (dialog#run ());
  dialog#destroy ()

(* Popup after export to specify a senator. *)
let get_senator_popup s =
  let dialog =
    GWindow.dialog ~title:"Provide a Senator Name!"
      ~width:width_of_senatordialog ~height:height_of_senatordialog
      ~destroy_with_parent:true ~position:`CENTER_ALWAYS ()
  in
  let entry = GEdit.entry ~width:width_of_entry ~height:height_of_entry () in
  entry#misc#modify_font_by_name "Times New Roman 24";
  let font_desc = Pango.Font.from_string "Georgia 28" in
  let label = GMisc.label ~markup:"<b>Export</b>" () in
  label#misc#modify_font font_desc;
  let button = GButton.button () in
  button#misc#set_size_request ~width:width_of_button ~height:height_of_button
    ();
  button#add label#coerce;
  let vbox =
    GPack.vbox ~packing:dialog#vbox#add
      ~spacing:spacing_between_entry_and_button ()
  in
  vbox#add entry#coerce;
  vbox#add button#coerce;

  ignore
    (button#connect#clicked ~callback:(fun () ->
         "Export " ^ s ^ "/senstalk.md " ^ entry#text |> handle |> display_popup));
  ignore (entry#connect#activate ~callback:(fun () -> button#clicked ()));
  ignore (entry#connect#changed ~callback:(fun () -> capitalize entry));

  dialog#show ();
  ignore (dialog#run ());
  dialog#destroy ()

let create_export_popup () =
  let selected w () =
    match w#filename with
    | Some s ->
        get_senator_popup s;
        print_endline s
    (* TODO change this*)
    | None -> ()
  in
  let widget = GWindow.file_chooser_dialog `SELECT_FOLDER () in
  (* let widget = GWindow.file_selection ~title:"File selection"
     ~border_width:10 () in *)
  print_endline "Export";
  widget#add_select_button_stock `OK `OPEN;
  widget#connect#destroy (selected widget) |> ignore;
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
    GWindow.window ~title:"Senate Stalker GUI"
      ~border_width:border_width_of_window ~width:width_of_window
      ~icon:icon_pixbuf ~height:height_of_window ()
  in
  ignore (window#connect#destroy ~callback:(fun () -> GMain.Main.quit ()));

  window#set_position `CENTER;
  window#set_resizable true;
  window#set_modal true;
  window#misc#realize ();

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
        `I ("History", fun () -> create_history_dialog ());
        `I ("Erase History", fun () -> create_erasehistory_dialog ());
        `I ("Help", fun () -> create_helppopup_dialog ());
        `I ("Export", fun () -> create_export_popup ());
        `S;
        `I ("Quit", GMain.Main.quit);
      ];

  menu#set_border_width 1;

  let datemenu =
    GMenu.menu
      ~packing:
        (GMenu.menu_item ~right_justified:true ~label:todays_date
           ~packing:menubar#append ())
          #set_submenu
      ()
  in
  GToolbox.build_menu datemenu
    ~entries:
      [
        `M ("Class I: End Term", classonedate);
        `M ("Class II: End Term", classtwodate);
        `M ("Class III: End Term", classthreedate);
        `S;
        `I ("Election", create_senateelectionpopup_dialog);
      ];
  datemenu#misc#set_sensitive true;

  let _ =
    GMisc.image
      ~pixbuf:
        (scale_pixbuf (GdkPixbuf.from_file "data/senatestalkerlogo.png") 64 64)
      ~height:40 ~packing:vbox#add ()
  in

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
         To save information on a specific senator, please enter <b>Export</b> \
         [<i>path directory</i>] [<i>full name of senator</i>] \n\
         Supported values of [<i>field</i>] are: \n\
        \    Name\n\
        \    Party\n\
        \    State\n\
        \    Address\n\
        \    Phone\n\
        \    Email\n\
        \    Website\n\
        \    Class\n\
        \    Stocks\n\
        \    Finance\n\
        \    Nominate\n\
        \    Approval\n\
        \    Committees\n"
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
  ignore (entry#connect#changed ~callback:(fun () -> capitalize entry));

  (* Shows a tool tip on the entry field *)
  let tooltips = GData.tooltips () in
  tooltips#set_tip entry#coerce ~text:"Type what you want to learn here!"
    ~privat:"Private";

  let button = GButton.button ~packing:hbox#add () in
  button#misc#set_size_request ~width:width_of_button ~height:height_of_button
    ();

  let font_desc = Pango.Font.from_string "Georgia 24" in
  let label = GMisc.label ~markup:"Stalk a <i>Senator</i>!" () in
  label#misc#modify_font font_desc;
  button#add label#coerce;

  (* Shows a tool tip on the button field *)
  let tooltips = GData.tooltips () in
  tooltips#set_tip button#coerce
    ~text:"Click the button or press enter to process your command!"
    ~privat:"Private";

  let scrolled_window =
    GBin.scrolled_window ~packing:vbox#add ~hpolicy:`AUTOMATIC
      ~vpolicy:`AUTOMATIC ~border_width:border_width_of_window
      ~width:normalwidth_of_resultlabel ~height:normalheight_of_resultlabel
      ~show:true ()
  in
  scrolled_window#set_shadow_type `OUT;
  scrolled_window#set_placement `TOP_LEFT;

  let result_label =
    GMisc.label ~text:"" ~width:normalwidth_of_resultlabel
      ~packing:scrolled_window#add_with_viewport ()
  in

  result_label#misc#modify_font_by_name "Serif 24";
  result_label#set_line_wrap true;
  result_label#set_selectable true;
  result_label#set_ellipsize `NONE;
  result_label#set_justify `CENTER;
  result_label#set_use_markup true;
  (* Connect the button click event to the callback function *)
  ignore
    (button#connect#clicked ~callback:(fun () ->
         button_callback entry result_label scrolled_window ()));

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
