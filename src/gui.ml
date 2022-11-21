module type S = sig
  type t

  val create : Common.cli_flags -> t
  val render : t -> unit
  val toggle_gui : t -> unit -> unit
  val set_pixel : t -> Common.set_pixel
  val render_raw : t -> unit
  val clear : t -> Stdint.uint8 -> unit
  val continue : t -> bool
  val shown : t -> bool
  val set_exit : t -> (unit -> unit) -> unit
  val set_save_state : t -> (Rom.Save_file.slot -> unit) -> unit
  val set_load_state : t -> (Rom.Save_file.slot -> unit) -> unit
  val exit : t -> unit
end

module type SF = functor (D : Display.S) -> S

module Disabled (D : Display.S) : S = struct
  type t = D.t

  let create cli_flags = Ppu_display.create D.create cli_flags
  let render_raw t = D.render t
  let clear = D.clear
  let set_pixel = D.set_pixel
  let render t = D.render t
  let toggle_gui _ _ = ()
  let continue _ = true
  let shown _ = false
  let set_exit _ _ = ()
  let set_save_state _ _ = ()
  let set_load_state _ _ = ()

  let exit t =
    D.delete t;
    D.exit ()
end

module Enabled (D : Display.S) : S = struct
  open Bogue
  module W = Widget
  module L = Layout

  type state = {
    mutable anim : bool;
    mutable continue : bool;
    mutable gui_shown : bool;
  }

  type gui = Main.board

  type callbacks = {
    mutable exit : unit -> unit;
    mutable save_state : Rom.Save_file.slot -> unit;
    mutable load_state : Rom.Save_file.slot -> unit;
  }

  type t = {
    board : gui;
    display : D.t;
    start : unit -> unit;
    fps : unit -> unit;
    state : state;
    callbacks : callbacks;
  }

  let set_pixel t = D.set_pixel t.display
  let render_raw t = D.render t.display
  let clear t = D.clear t.display
  let continue t = t.state.continue
  let shown t = t.state.gui_shown
  let set_exit t f = t.callbacks.exit <- f
  let set_save_state t f = t.callbacks.save_state <- f
  let set_load_state t f = t.callbacks.load_state <- f

  let create_board window callbacks =
    let dummy () = Printf.printf "GUI action not implemented yet!\n%!" in
    let exit () = raise Bogue.Exit in
    let save_call slot () =
      callbacks.save_state slot;
      exit ()
    in
    let load_call slot () =
      callbacks.load_state slot;
      exit ()
    in
    let state_entries f =
      Menu.Flat
        Rom.Save_file.
          [
            { label = Text "Slot 1"; content = Action (f S1) };
            { label = Text "Slot 2"; content = Action (f S2) };
            { label = Text "Slot 3"; content = Action (f S3) };
          ]
    in
    let entries =
      Menu.
        [
          {
            label = Text "Emulation";
            content =
              Tower
                [
                  { label = Text "Reset"; content = Action dummy };
                  { label = Text "Quit"; content = Action callbacks.exit };
                ];
          };
          {
            label = Text "State";
            content =
              Tower
                [
                  {
                    label = Text "Save state";
                    content = state_entries save_call;
                  };
                  {
                    label = Text "Load state";
                    content = state_entries load_call;
                  };
                ];
          };
          {
            label = Text "Settings";
            content =
              Tower
                [
                  { label = Text "Control mapping"; content = Action dummy };
                  {
                    label = Text "Toggle debug windows";
                    content = Action dummy;
                  };
                ];
          };
          { label = Text "About"; content = Action dummy };
        ]
    in
    let layout = Layout.empty ~w:800 ~h:100 () in
    Menu.add_bar ~dst:layout entries;
    let board =
      Bogue.of_layout
        ~shortcuts:Bogue.(shortcuts_of_list [ exit_on_escape ])
        layout
    in
    Bogue.make_sdl_windows ~windows:[ window ] board;
    let start, fps = Time.adaptive_fps 60 in
    start ();
    (board, start, fps)

  let create cli_flags =
    let display = Ppu_display.create D.create cli_flags in
    let state = { anim = false; gui_shown = false; continue = true } in
    let window = D.get_window display in
    let callbacks =
      {
        exit = (fun () -> state.continue <- false);
        save_state = (fun _ -> Printf.printf "Dummy save\n");
        load_state = (fun _ -> Printf.printf "Dummy load\n");
      }
    in
    let board, start, fps = create_board window callbacks in
    { board; start; fps; state; display; callbacks }

  let render_gui t =
    try
      Bogue.refresh_custom_windows t.board;
      t.state.anim <- Bogue.one_step t.state.anim (t.start, t.fps) t.board;
      if not t.state.anim then t.fps ();
      `Continue
    with Bogue.Exit -> `Exited

  let toggle_gui t () = t.state.gui_shown <- not t.state.gui_shown

  let render t =
    let after () =
      if t.state.gui_shown then
        match render_gui t with `Continue -> () | `Exited -> toggle_gui t ()
    in
    D.render ~after t.display

  let exit t =
    D.delete t.display;
    D.exit ()
end
