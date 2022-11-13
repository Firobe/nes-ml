module Keys = struct
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

  let compare = Stdlib.compare

  let to_string = function
    | A -> "A"
    | B -> "B"
    | Right -> "Right"
    | Left -> "Left"
    | Down -> "Down"
    | Up -> "Up"
    | Start -> "Start"
    | Select -> "Select"
    | _ -> "unknown"
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

module Make (B : Backend) : S with type backend = B.t = struct
  type t = { mutable next_key : int; backend_state : B.t }
  type backend = B.t

  let create () = { next_key = 0; backend_state = B.create () }

  let nes_key_order =
    Array.of_list Keys.[ A; B; Select; Start; Up; Down; Left; Right ]

  let next_nes_key t =
    let to_check = nes_key_order.(t.next_key) in
    t.next_key <- (t.next_key + 1) mod 8;
    B.key_pressed t.backend_state to_check

  let next_register t = if next_nes_key t then 1u else 0u
  let next_frame t = B.next_frame t.backend_state
  let get_inputs t callbacks = B.get_inputs t.backend_state callbacks
end
