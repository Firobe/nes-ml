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

let key_pressed key =
    let scancode = mapping key in
    state.{scancode} != 0

let continue () =
    state.{Sdl.Scancode.escape} == 0

let rec get_inputs () =
    let event = Sdl.Event.create () in
    if Sdl.poll_event (Some event) then
        get_inputs ()

let nes_input_state = ref 0

let nes_key_order = Array.of_list [A; B; Select; Start; Up; Down; Left; Right]

let next_nes_key () =
    let to_check = nes_key_order.(!nes_input_state) in
    nes_input_state := (!nes_input_state + 1) mod 8;
    key_pressed to_check

let next_register () =
    if next_nes_key () then 1 else 0
