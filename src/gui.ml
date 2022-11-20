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
  display : Display.t;
  start : unit -> unit;
  fps : unit -> unit;
  state : state;
  callbacks : callbacks;
}

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
                { label = Text "Save state"; content = state_entries save_call };
                { label = Text "Load state"; content = state_entries load_call };
              ];
        };
        {
          label = Text "Settings";
          content =
            Tower
              [
                { label = Text "Control mapping"; content = Action dummy };
                { label = Text "Toggle debug windows"; content = Action dummy };
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
  let display = Ppu_display.create cli_flags in
  let state = { anim = false; gui_shown = false; continue = true } in
  let window = Display.get_window display in
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
  Display.render ~after t.display

let exit t =
  Display.delete t.display;
  Display.exit ()
