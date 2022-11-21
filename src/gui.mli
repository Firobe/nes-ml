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

  val set_pixel : t -> Common.set_pixel
  val render_raw : t -> unit
  val clear : t -> Stdint.uint8 -> unit
  val continue : t -> bool
  val shown : t -> bool
  val set_exit : t -> (unit -> unit) -> unit
  val set_save_state : t -> (Rom.Save_file.slot -> unit) -> unit
  val set_load_state : t -> (Rom.Save_file.slot -> unit) -> unit

  val exit : t -> unit
  (** Exit and destroy the main window *)
end

module type SF = functor (D : Display.S) -> S

module Enabled : SF
module Disabled : SF
