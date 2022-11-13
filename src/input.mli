(** Manage keyboard IO for the NES and the emulator itself *)

module Keys : sig
  type t =
    | A
    | B
    | Right
    | Left
    | Down
    | Up
    | Start
    | Select
    | Toggle_debug
    | Toggle_gui
    | Save_state of Rom.Save_file.slot
    | Load_state of Rom.Save_file.slot

  val compare : t -> t -> int
end

type t
(** State of the input state machine *)

type callbacks = {
  toggle_debug : unit -> unit;
  toggle_gui : unit -> unit;
  save_state : Rom.Save_file.slot -> unit;
  load_state : Rom.Save_file.slot -> unit;
}

module type Backend = sig
  val key_pressed : Keys.t -> bool
  val get_inputs : callbacks -> unit
end

val create : (module Backend) -> t

val next_register : t -> Stdint.uint8
(** Value of the next input register for the NES *)

val get_inputs : t -> callbacks -> unit
(** Call back the functions if the related input is triggered *)
