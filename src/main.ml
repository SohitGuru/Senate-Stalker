(* [run_stalking s] starts the web scraping of Senator*)
let run_stalking s = raise (Failure "Error!")

(** [main ()] prompts for the Senator name, then starts finding information
    about said Senator. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nSenate Stalker: Learn more about our Senators!\n";
  print_endline
    "Please enter the name of the Senator you wish to learn more about.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> run_stalking "Senator"

(* Execute finding Senator following input. *)
let () = main ()
