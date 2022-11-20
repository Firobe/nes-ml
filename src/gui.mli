(** Full definition of the emulator's GUI and rendering window *)

type state = {
  mutable anim : bool;
  mutable continue : bool;
  mutable gui_shown : bool;
}

type gui

type callbacks = {
  mutable exit : unit -> unit;
  mutable save_state : Rom.Save_file.slot -> unit;
  mutable load_state : Rom.Save_file.slot -> unit;
}

type t = {
  board : gui;
  display : Display.t;
  start : unit -> unit;
  fps : unit -> unit;
  state : state;
  callbacks : callbacks;
}

val create : Common.cli_flags -> t
(** Create the emulator GUI attached to a window, and callbacks to call *)

val render : t -> unit
(** Refresh and render the GUI to the attached window. Return if the GUI has
    exited *)

val toggle_gui : t -> unit -> unit
(** Toggle if GUI should be shown or not *)

val exit : t -> unit
(** Exit and destroy the main window *)
