open Senate
open Command

let run_stalking s = raise (Failure "Error!")

let help_message =
  "To get a list of senators, please enter the command \'list\'\n\
   To get information on a specific senator, please enter the field you want, \
   followed by the senator's full name.\n\
   Supported information fields are: \n\
  \    state\n\
  \    party\n\
  \    committees\n"

let prompt = "> "

let handle cmd =
  let open Command in
  match cmd |> parse with
  | (exception Empty) | (exception Invalid) -> "Invalid or empty argument"
  | Quit -> raise Exit
  | List -> Executor.execute List |> String.concat "\n"
  | Fetch (f, o) -> Executor.execute (Fetch (f, o)) |> String.concat "\n"

let rec prompter () =
  print_string prompt;
  match read_line () with
  | exception End_of_file -> ()
  | st -> (
      match handle st with
      | exception Exit -> ()
      | out ->
          print_endline out;
          prompter ())

(** [main ()] prompts for the Senator name, then starts finding information
    about said Senator. *)
let main () =
  ANSITerminal.print_string
    [ ANSITerminal.blue; ANSITerminal.Bold ]
    "\n\nSenate Stalker: Learn more about our Senators!\n";
  ANSITerminal.print_string [ ANSITerminal.yellow ] help_message;
  prompter ();
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nThank you for using Senate Stalker\n"

(* Execute finding Senator following input. *)
let () = main ()
