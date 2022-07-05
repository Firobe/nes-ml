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
    | Debug
    | Save_state of Rom.Save_file.slot
    | Load_state of Rom.Save_file.slot
end

type t
(** State of the input state machine *)

val create : unit -> t

val next_register : t -> Stdint.uint8
(** Value of the next input register for the NES *)

val continue : unit -> bool
(** The game loop should continue *)

type callbacks = {
  debug : unit -> unit;
  save_state : Rom.Save_file.slot -> unit;
  load_state : Rom.Save_file.slot -> unit;
}

val get_inputs : callbacks -> unit
(** Call back the functions if the related input is triggered *)
