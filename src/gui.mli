(** Full definition of the emulator's GUI and rendering window *)

module type S = sig
  type t

  val create : Common.cli_flags -> t
  (** Create the emulator GUI attached to a window, and callbacks to call *)

  val render : t -> unit
  (** Refresh and render the GUI to the attached window. Return if the GUI has
      exited *)

  val toggle_gui : t -> unit -> unit
  (** Toggle if GUI should be shown or not *)

  val continue : t -> bool
  val shown : t -> bool
  val set_exit : t -> (unit -> unit) -> unit
  val set_save_state : t -> (Rom.Save_file.slot -> unit) -> unit
  val set_load_state : t -> (Rom.Save_file.slot -> unit) -> unit
  val display : t -> Display.t

  val exit : t -> unit
  (** Exit and destroy the main window *)
end

module Enabled : S
module Disabled : S
