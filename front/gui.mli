(** GUI file and functions *)

val create_window : unit -> GWindow.window
(** Creates and displays a GUI window *)

val create_button : GPack.fixed -> GButton.button
(** Creates a button and handle its click event *)

val main : unit -> unit
(** Entry point of the GUI application *)
