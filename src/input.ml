open Tsdl

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

module Keymap = Map.Make(Keys)

type binding = {
  key : Sdl.keycode;
  kmod : Sdl.keymod
}

(* Hardcoded, for now *)
let bindings =
  let open Sdl.K in
  (* Bare key *)
  let b k = {key = k; kmod = Sdl.Kmod.none} in
  (* Key with shift modifier *)
  let shift k = {key = k; kmod = Sdl.Kmod.lshift} in
  let open Keys in
  [(A, b s); (B, b d); (Right, b right); (Left, b left);
   (Down, b down); (Up, b up); (Start, b return); (Select, b backspace);
   (Toggle_debug, b home); (Save_state S1, b k1); (Save_state S2, b k2);
   (Save_state S3, b k3); (Load_state S1, shift k1); (Load_state S2, shift k2);
   (Load_state S3, shift k3); (Toggle_gui, b escape)]
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

type callbacks = {
  toggle_debug : unit -> unit;
  toggle_gui : unit -> unit;
  save_state : Rom.Save_file.slot -> unit;
  load_state : Rom.Save_file.slot -> unit;
}

let create () = {next_key = 0}

let state = Sdl.get_keyboard_state ()

let key_pressed key =
  let {key; _} = Keymap.find key bindings in
  let scancode = Sdl.get_scancode_from_key key in
  state.{scancode} != 0

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
            | Some Toggle_debug -> c.toggle_debug ()
            | Some Toggle_gui -> c.toggle_gui ()
            | Some Save_state slot -> c.save_state slot
            | Some Load_state slot -> c.load_state slot
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
