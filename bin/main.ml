open Senate
open Command

let run_stalking s = raise (Failure "Error!")

let help_message =
  "To get a list of senators, please enter the command \'List\'\n\
   To get information on a specific senator, please enter \'Fetch [field] \
   [full name of senator] \'\n\
   To save information on a specific senator, please enter \'Export [path \
   directory] [full name of senator] \'\n\
   Supported values of [field] are: \n\
  \    Name\n\
  \    Party\n\
  \    State\n\
  \    Address\n\
  \    Phone\n\
  \    Email\n\
  \    Website\n\
  \    Class\n\
  \    Nominate\n\
  \    Committees\n"

let prompt = "> "

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
  | Export (p, s) -> (
      try String.concat "\n" (Executor.execute (Export (p, s)))
      with _ -> "An error occured")

let rec prompter () =
  print_string prompt;
  match read_line () with
  | exception End_of_file -> ()
  | st -> (
      match handle st with
      | exception Exit -> ()
      | out ->
          ANSITerminal.print_string [ ANSITerminal.green ] (out ^ "\n");
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
