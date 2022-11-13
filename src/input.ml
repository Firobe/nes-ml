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
end

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

type t = {
  backend : (module Backend);
  mutable next_key : int;
}

let create backend = {
  next_key = 0;
  backend
}

let nes_key_order =
  Array.of_list Keys.[ A; B; Select; Start; Up; Down; Left; Right ]

let next_nes_key t =
  let to_check = nes_key_order.(t.next_key) in
  t.next_key <- (t.next_key + 1) mod 8;
  let module B = (val t.backend : Backend) in
  B.key_pressed to_check

let next_register t = if next_nes_key t then 1u else 0u

let get_inputs t callbacks =
  let module B = (val t.backend : Backend) in
  B.get_inputs callbacks
