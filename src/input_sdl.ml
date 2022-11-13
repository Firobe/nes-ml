open Tsdl

module Keymap = Map.Make (Input.Keys)

type binding = { key : Sdl.keycode; kmod : Sdl.keymod }

(* Hardcoded, for now *)
let bindings =
  let open Sdl.K in
  (* Bare key *)
  let b k = { key = k; kmod = Sdl.Kmod.none } in
  (* Key with shift modifier *)
  let shift k = { key = k; kmod = Sdl.Kmod.lshift } in
  let open Input.Keys in
  [
    (A, b s);
    (B, b d);
    (Right, b right);
    (Left, b left);
    (Down, b down);
    (Up, b up);
    (Start, b return);
    (Select, b backspace);
    (Toggle_debug, b home);
    (Save_state S1, b k1);
    (Save_state S2, b k2);
    (Save_state S3, b k3);
    (Load_state S1, shift k1);
    (Load_state S2, shift k2);
    (Load_state S3, shift k3);
    (Toggle_gui, b escape);
  ]
  |> List.to_seq |> Keymap.of_seq

let reverse_binding b =
  let f key b' found =
    match found with
    | None -> if b = b' then Some key else None
    | Some _ -> found
  in
  Keymap.fold f bindings None

let state = Sdl.get_keyboard_state ()

let key_pressed key =
  let { key; _ } = Keymap.find key bindings in
  let scancode = Sdl.get_scancode_from_key key in
  state.{scancode} != 0

let get_inputs (c : Input.callbacks) =
  let open Sdl in
  let event = Event.create () in
  let rec aux () =
    if poll_event (Some event) then (
      let typ = Event.get event Event.typ in
      (match Event.enum typ with
      | `Key_down -> (
          let key = Event.get event Event.keyboard_keycode in
          let kmod = Event.get event Event.keyboard_keymod in
          let binding = { key; kmod } in
          match reverse_binding binding with
          | Some Toggle_debug -> c.toggle_debug ()
          | Some Toggle_gui -> c.toggle_gui ()
          | Some (Save_state slot) -> c.save_state slot
          | Some (Load_state slot) -> c.load_state slot
          | _ -> ())
      | _ -> ());
      aux ())
  in
  aux ()

