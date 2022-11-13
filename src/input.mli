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
  val to_string : t -> string
end

type callbacks = {
  toggle_debug : unit -> unit;
  toggle_gui : unit -> unit;
  save_state : Rom.Save_file.slot -> unit;
  load_state : Rom.Save_file.slot -> unit;
}

module type Backend = sig
  type t

  val create : unit -> t
  val key_pressed : t -> Keys.t -> bool
  val get_inputs : t -> callbacks -> unit
  val next_frame : t -> unit
end

module type S = sig
  type t
  (** State of the input state machine *)

  type backend

  val create : unit -> t

  val next_register : t -> Stdint.uint8
  (** Value of the next input register for the NES *)

  val next_frame : t -> unit

  val get_inputs : t -> callbacks -> unit
  (** Call back the functions if the related input is triggered *)
end

module Make : functor (B : Backend) -> S with type backend = B.t
