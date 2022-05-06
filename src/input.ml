open Tsdl

type nes_input =
  | A
  | B
  | Right
  | Left
  | Down
  | Up
  | Start
  | Select
  | Debug
  | Save_state
  | Load_state

let mapping = function
  | A -> Sdl.Scancode.s
  | B -> Sdl.Scancode.d
  | Right -> Sdl.Scancode.right
  | Left -> Sdl.Scancode.left
  | Down -> Sdl.Scancode.down
  | Up -> Sdl.Scancode.up
  | Start -> Sdl.Scancode.return
  | Select -> Sdl.Scancode.backspace
  | Debug -> Tsdl.Sdl.Scancode.home
  | Save_state -> Tsdl.Sdl.Scancode.i
  | Load_state -> Tsdl.Sdl.Scancode.o


type t = {
  mutable next_key : int;
}

type callback = unit -> unit

type callbacks = {
  debug : callback;
  save_state : callback;
  load_state : callback;
}

let create () = {next_key = 0}

let state = Sdl.get_keyboard_state ()

let key_pressed key =
  let scancode = mapping key in
  state.{scancode} != 0

let continue () =
  state.{Sdl.Scancode.escape} == 0

let get_inputs (c : callbacks) =
  let open Sdl in
  let event = Event.create () in
  let rec aux () =
    if poll_event (Some event) then (
      let typ = Event.get event Event.typ in
      begin match Event.enum typ with
        | `Key_up ->
          let scancode = Event.get event Event.keyboard_scancode in
          if scancode = mapping Debug then c.debug ()
          else if scancode = mapping Save_state then c.save_state ()
          else if scancode = mapping Load_state then c.load_state ()
        | _ -> ()
      end;
      aux ()
    )
  in aux ()

let nes_key_order = Array.of_list [A; B; Select; Start; Up; Down; Left; Right]

let next_nes_key t =
  let to_check = nes_key_order.(t.next_key) in
  t.next_key <- (t.next_key + 1) mod 8;
  key_pressed to_check

let next_register t =
  if next_nes_key t then 1u else 0u
