open Tsdl

let state = Sdl.get_keyboard_state ()

let continue () =
    state.{Sdl.Scancode.escape} == 0

let rec get_inputs () =
    let event = Sdl.Event.create () in
    if Sdl.poll_event (Some event) then
        get_inputs ()
