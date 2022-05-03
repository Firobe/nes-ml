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
  | Debug_on
  | Debug_off
  | Save_state
  | Load_state

let state = Sdl.get_keyboard_state ()

let mapping = function
  | A -> Sdl.Scancode.s
  | B -> Sdl.Scancode.d
  | Right -> Sdl.Scancode.right
  | Left -> Sdl.Scancode.left
  | Down -> Sdl.Scancode.down
  | Up -> Sdl.Scancode.up
  | Start -> Sdl.Scancode.return
  | Select -> Sdl.Scancode.backspace
  | Debug_on -> Tsdl.Sdl.Scancode.home
  | Debug_off -> Tsdl.Sdl.Scancode.kend
  | Save_state -> Tsdl.Sdl.Scancode.i
  | Load_state -> Tsdl.Sdl.Scancode.o

let key_pressed key =
  let scancode = mapping key in
  state.{scancode} != 0

let continue () =
  state.{Sdl.Scancode.escape} == 0

let rec get_inputs () =
  let event = Sdl.Event.create () in
  if Sdl.poll_event (Some event) then
    get_inputs ()

type t = {mutable next_key : int}
let create () = {next_key = 0}

let nes_key_order = Array.of_list [A; B; Select; Start; Up; Down; Left; Right]

let next_nes_key t =
  let to_check = nes_key_order.(t.next_key) in
  t.next_key <- (t.next_key + 1) mod 8;
  key_pressed to_check

let next_register t =
  if next_nes_key t then 1u else 0u
