open Bogue
module W = Widget
module L = Layout

type state = {
  mutable anim : bool;
  mutable continue : bool;
  mutable gui_shown : bool
}

type gui = Main.board

type t = {
  board : gui;
  display : Display.t;
  start : unit -> unit;
  fps : unit -> unit;
  state : state
}

type callbacks = {
  exit : unit -> unit;
}

let create_board window callbacks =
  let dummy () = Printf.printf "GUI action not implemented yet!\n%!" in
  let states_entries = Menu.Flat [
      {label = Text "Slot 1"; content = Action dummy};
      {label = Text "Slot 2"; content = Action dummy};
      {label = Text "Slot 3"; content = Action dummy};
    ] in
  let entries = Menu.[
      {label = Text "Emulation"; content = Tower [
           {label = Text "Reset"; content = Action dummy};
           {label = Text "Quit"; content = Action callbacks.exit}
         ]};
      {label = Text "State"; content = Tower [
           {label = Text "Save state"; content = states_entries};
           {label = Text "Load state"; content = states_entries}
         ]};
      {label = Text "Settings"; content = Tower [
           {label = Text "Control mapping"; content = Action dummy};
           {label = Text "Toggle debug windows"; content = Action dummy};
         ]};
      {label = Text "About"; content = Action dummy};
    ] in
  let layout = Layout.empty ~w:800 ~h:100 () in
  Menu.add_bar ~dst:layout entries;
  let board = Bogue.of_layout ~shortcuts:Bogue.(
      shortcuts_of_list [exit_on_escape]
    ) layout in
  Bogue.make_sdl_windows ~windows:[window] board;
  let start, fps = Time.adaptive_fps 60 in
  start ();
  board, start, fps

let create () =
  let display = Ppu_display.create () in
  let state = {
    anim = false;
    gui_shown = false;
    continue = true
  } in
  let window = Display.get_window display in
  let callbacks = {
    exit = fun () -> state.continue <- false
  } in
  let board, start, fps = create_board window callbacks in
  {board; start; fps; state; display}

let render_gui t =
  try
    Bogue.refresh_custom_windows t.board;
    t.state.anim <- Bogue.one_step t.state.anim (t.start, t.fps) t.board;
    if not t.state.anim then t.fps ();
    `Continue
  with Bogue.Exit -> `Exited

let toggle_gui t () =
  t.state.gui_shown <- not t.state.gui_shown

let render t =
  let after () =
    if t.state.gui_shown then
      match render_gui t with
      | `Continue -> ()
      | `Exited -> toggle_gui t ()
  in
  Display.render ~after t.display

let exit t =
  Display.delete t.display;
  Display.exit ()