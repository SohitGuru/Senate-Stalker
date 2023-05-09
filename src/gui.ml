let create_window () =
  let window =
    GWindow.window ~title:"Senate Stalker GUI" ~width:500 ~height:300 ()
  in
  ignore (window#connect#destroy ~callback:(fun () -> GMain.Main.quit ()));
  window

let create_button _ =
  let button = GButton.button ~label:"Press!" () in
  ignore (button#connect#clicked ~callback:(fun () -> print_endline "hey"));
  button

let main () =
  ignore (GtkMain.Main.init ());
  let window = create_window () in
  let button = create_button window in
  window#add button#coerce;
  window#show ();
  GtkMain.Main.main ()

let () = main ()
