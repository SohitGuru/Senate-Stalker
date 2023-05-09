(** GUI file and functions *)

val create_window : unit -> GEdit.entry * GButton.button
(** Create the main window with a label, text field entry, and button. Returns a
    tuple containing the entry widget and the button widget. *)

val create_button : GPack.fixed -> GButton.button
(** Creates a button and handle its click event *)

val handle_button_click : GEdit.entry -> unit
(** Handle the button click event and retrieve the text from the entry widget. *)

val main : unit -> unit
(** Entry point of the GUI application. Executes GUI *)
