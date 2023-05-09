open Senate
open Command

(* Constants to avoid use of magic numbers*)
let width_of_window = 800
let height_of_window = 650
let x_coordinate_of_button = 10
let y_coordinate_of_button = 10
let max_length_of_text = 10
let width_of_button = 50
let height_of_button = 80

let handle cmd =
  let open Command in
  match cmd |> parse with
  | (exception Empty) | (exception Invalid) -> "Invalid or empty argument"
  | Quit -> raise Exit
  | List -> Executor.execute List |> String.concat "\n"
  | Fetch (f, o) -> (
      let open Executor in
      try execute (Fetch (f, o)) |> String.concat "\n" with
      | BadArgument -> "Invalid senator"
      | UnexpectedError -> "An unexpected error occurred while webscraping.")

let rec prompter text =
  print_string "> ";
  match text with
  | exception End_of_file -> ()
  | text -> (
      match handle text with
      | exception Exit -> ()
      | out -> print_endline (out ^ "\n"))

(* Initial window without any widgets *)
let create_window () =
  let window =
    GWindow.window ~title:"Senate Stalker GUI" ~width:width_of_window
      ~height:height_of_window ()
  in
  ignore (window#connect#destroy ~callback:(fun () -> GMain.Main.quit ()));

  let vbox = GPack.vbox ~packing:window#add () in

  let _ = GMisc.label ~text:"Welcome" ~packing:vbox#add () in

  let hbox = GPack.hbox ~packing:vbox#add () in

  let entry = GEdit.entry ~max_length:max_length_of_text ~packing:hbox#add () in

  let button = GButton.button ~label:"Stalk!" ~packing:hbox#add () in
  button#misc#set_size_request ~width:width_of_button ~height:height_of_button
    ();

  window#show ();
  (entry, button)

(* Button with constraints on size *)
let create_button fixed_container =
  let button = GButton.button ~label:"Test press" () in
  ignore (button#connect#clicked ~callback:(fun () -> print_endline "Sample"));
  fixed_container#put button#coerce ~x:x_coordinate_of_button
    ~y:y_coordinate_of_button;
  button

(* Retreive text written by client *)
let handle_button_click entry =
  let text = entry#text in
  prompter text

(* Build up window *)
let main () =
  ignore (GMain.init ());
  let entry, button = create_window () in
  ignore
    (button#connect#clicked ~callback:(fun () -> handle_button_click entry));
  GMain.main ()

let () = main ()
