open Tsdl

module M = struct
  type t = unit

  module Keymap = Map.Make (Input.Keys)

  type binding = { key : Sdl.keycode; kmod : Sdl.keymod }

  let create () = ()

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

  let key_pressed () key =
    let { key; _ } = Keymap.find key bindings in
    let scancode = Sdl.get_scancode_from_key key in
    state.{scancode} != 0

  let gen_get_inputs key_callback (c : Input.callbacks) =
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
            | Some k -> key_callback k
            | None -> ())
        | _ -> ());
        aux ())
    in
    aux ()

  let get_inputs () (c : Input.callbacks) = gen_get_inputs (fun _ -> ()) c
  let next_frame () = ()
end

module type Out = sig
  val file : string
end

module KSet = Set.Make (Input.Keys)

let fm2_write_line channel ks kp =
  let p = Printf.fprintf channel in
  let pk k c =
    if KSet.mem k ks || kp k then Printf.fprintf channel "%c" c
    else Printf.fprintf channel "."
  in
  p "|0|";
  pk Right 'R';
  pk Left 'L';
  pk Down 'D';
  pk Up 'U';
  pk Start 'T';
  pk Select 'S';
  pk B 'B';
  pk A 'A';
  p "|||\n"

module Make_record (O : Out) = struct
  type t = { mutable this_frame : KSet.t; channel : out_channel }

  let create () =
    let channel = open_out O.file in
    Printf.fprintf channel
      {|version 3
emuVersion 20604
palFlag 0
romFilename mario
romChecksum base64:ujnd5jqyCbG8dR4FNecrGA==
guid 79C0CEC5-3EB1-9373-34DA-53BEE986562A
fourscore 0
microphone 0
port0 1
port1 1
port2 0
FDS 0
NewPPU 0
|};
    { this_frame = KSet.empty; channel }

  let key_pressed _ = M.key_pressed ()

  let get_inputs t c =
    let add_key_to_frame k = t.this_frame <- KSet.add k t.this_frame in
    M.gen_get_inputs add_key_to_frame c

  let next_frame t =
    fm2_write_line t.channel t.this_frame (M.key_pressed ());
    t.this_frame <- KSet.empty
end

include M
