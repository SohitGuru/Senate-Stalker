let create_window () =
  let window =
    GWindow.window ~title:"Senate Stalker GUI" ~width:500 ~height:500 ()
  in
  ignore (window#connect#destroy ~callback:(fun () -> GMain.Main.quit ()));
  window

let create_button fixed_container =
  let button = GButton.button ~label:"Test press" () in
  ignore (button#connect#clicked ~callback:(fun () -> print_endline "Sample"));
  fixed_container#put button#coerce ~x:0 ~y:0;
  button

let main () =
  ignore (GtkMain.Main.init ());
  let window = create_window () in
  let fixed_container = GPack.fixed () in
  window#add fixed_container#coerce;
  let _ = create_button fixed_container in
  window#show ();
  GtkMain.Main.main ()

let () = main ()
