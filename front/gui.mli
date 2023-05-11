(** GUI file and functions *)

val create_window : unit -> GEdit.entry * GButton.button * GMisc.label
(** Create the main window with a label, text field entry, and button. Returns a
    tuple containing the entry widget and the button widget. *)

val create_button : GPack.fixed -> GButton.button
(** Creates a button and handle its click event *)

val main : unit -> unit
(** Entry point of the GUI application. Executes GUI *)
