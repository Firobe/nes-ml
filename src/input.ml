open Tsdl

module Keys = struct
  type save_slot = S1 | S2 | S3

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
    | Save_state of save_slot
    | Load_state of save_slot

  let compare = Stdlib.compare
end

module Keymap = Map.Make(Keys)

type binding = {
  key : Sdl.keycode;
  kmod : Sdl.keymod
}

(* Hardcoded, for now *)
let bindings =
  let open Sdl.K in
  let b k = {key = k; kmod = Sdl.Kmod.none} in
  let shift k = {key = k; kmod = Sdl.Kmod.lshift} in
  let open Keys in
  [(A, b s); (B, b d); (Right, b right); (Left, b left);
   (Down, b down); (Up, b up); (Start, b return); (Select, b backspace);
   (Debug, b home); (Save_state S1, b k1); (Save_state S2, b k2);
   (Save_state S3, b k3); (Load_state S1, shift k1); (Load_state S2, shift k2);
   (Load_state S3, shift k3)]
  |> List.to_seq
  |> Keymap.of_seq

let reverse_binding b =
  let f key b' found = match found with
    | None -> if b = b' then Some key else None
    | Some _ -> found
  in
  Keymap.fold f bindings None

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
  let {key; _} = Keymap.find key bindings in
  let scancode = Sdl.get_scancode_from_key key in
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
        | `Key_down ->
          let key = Event.get event Event.keyboard_keycode in
          let kmod = Event.get event Event.keyboard_keymod in
          let binding = {key; kmod} in
          begin match reverse_binding binding with
            | Some Debug -> c.debug ()
            | Some Save_state _ -> c.save_state ()
            | Some Load_state _ -> c.load_state ()
            | _ -> ()
          end
        | _ -> ()
      end;
      aux ()
    )
  in aux ()

let nes_key_order = Array.of_list Keys.[A; B; Select; Start; Up; Down; Left; Right]

let next_nes_key t =
  let to_check = nes_key_order.(t.next_key) in
  t.next_key <- (t.next_key + 1) mod 8;
  key_pressed to_check

let next_register t =
  if next_nes_key t then 1u else 0u
