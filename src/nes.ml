open Common

type ('ppu, 'apu, 'input, 'mapper) devices = {
  rom : Rom.t;
  mapper : 'mapper;
  apu : 'apu;
  ppu : 'ppu;
  input : 'input;
}

module Build_NES (P : Ppu.S) (A : Apu.S) (M : Mapper.S) (I : Input.S) :
  C6502.MemoryMap with type input = (P.t, A.t, I.t, M.t) devices = struct
  open Infix_int.Common

  type input = (P.t, A.t, I.t, M.t) devices

  type t = {
    main : U8.t array;
    mapper : M.t;
    apu : A.t;
    ppu : P.t;
    input : I.t;
  }

  let create ({ mapper; apu; ppu; input; _ } : input) =
    { main = Array.make 0x8000 0u; mapper; input; apu; ppu }

  (* Utils *)
  let is_in_ppu_range addr = addr >= 0x2000U && addr <= 0x2007U

  let is_in_apu_range addr =
    addr >= 0x4000U && addr <= 0x4017U && addr <> 0x4014U

  let is_in_cartridge_range addr = addr >= 0x8000U

  let address_mirroring a =
    let open U16 in
    if a < 0x2000U then (* RAM mirroring *)
      a $& 0x07FFU
    else if a $>> 13 = 1U then
      (* PPU mirroring *)
      (* TODO also mask ?*)
      a $& 0x2007U
    else a

  let read t (a : U16.t) : U8.t =
    let open U16 in
    let a = address_mirroring a in
    if is_in_ppu_range a then P.get_register t.ppu (to_int (logand a 7U))
    else if a = 0x4015U then A.read_register t.apu a
    else if a = 0x4016U then I.next_register t.input
    else if is_in_cartridge_range a then M.CPU.read t.mapper a
    else t.main.(?%a)

  let write t (a : U16.t) (v : U8.t) =
    let open U16 in
    let a = address_mirroring a in
    if is_in_ppu_range a then P.set_register t.ppu (to_int (logand a 7U)) v
    else if is_in_apu_range a then A.write_register t.apu v a
    else if a = 0x4014U then P.dma t.ppu (read t) (?$v $<< 8)
    else if is_in_cartridge_range a then M.CPU.write t.mapper a v
    else t.main.(?%a) <- v
end

module Main
    (P : Ppu.S)
    (G : Gui.S)
    (A : Apu.S)
    (M : Mapper.S)
    (I : Input.S)
    (NES : C6502.CPU with type input := (P.t, A.t, I.t, M.t) devices) =
struct
  type state = {
    cpu : NES.t;
    apu : A.t;
    ppu : P.t;
    rom : Rom.t;
    collector : C6502.IRQ_collector.t;
    input : I.t;
    cli_flags : cli_flags;
  }

  type io = { mutable debug : P.Debug.t option; main_window : G.t }
  type t = { mutable state : state; io : io }

  module Save_state = struct
    let save t slot =
      try
        let save_name = Rom.Save_file.make_name t.state.rom slot in
        let chan = open_out_bin save_name in
        Marshal.(to_channel chan t.state []);
        close_out chan
      with Sys_error err -> Printf.printf "Cannot save state: %s\n%!" err

    let load t slot =
      let err msg = Printf.printf "Cannot load state: %s\n%!" msg in
      try
        match Rom.Save_file.find_matching_name t.state.rom slot with
        | Some path ->
            let chan = open_in_bin path in
            let state' = Marshal.from_channel chan in
            t.state <- state';
            close_in chan
        | None -> err "no save file existing."
      with
      | Sys_error msg -> err msg
      | Failure msg ->
          Printf.printf
            "Cannot parse save state (%s). It was probably saved with a \
             different version of the emulator.\n\
             %!"
            msg
  end

  let create ({ apu; ppu; input; rom; _ } as d : (P.t, A.t, I.t, M.t) devices)
      collector nmi cli_flags =
    let cpu = NES.create ~collector ~nmi d in
    NES.Register.set (NES.registers cpu) `S 0xFDu;
    NES.Register.set (NES.registers cpu) `P 0x34u;
    NES.PC.init (NES.pc cpu) (NES.memory cpu);
    NES.enable_decimal cpu false;
    let state = { cpu; apu; ppu; collector; input; cli_flags; rom } in
    let io = { debug = None; main_window = G.create cli_flags } in
    { state; io }

  let debug_callback t () =
    match t.io.debug with
    | None -> t.io.debug <- Some (P.Debug.create ())
    | Some d ->
        P.Debug.delete d;
        t.io.debug <- None

  let run t =
    (* When the GUI is enabled, wait until the current NES frame is drawn before
       pausing, to avoid having a partial image *)
    let enable_gui_at_next_frame = ref false in
    let callbacks =
      Input.
        {
          toggle_debug = debug_callback t;
          save_state = Save_state.save t;
          load_state = Save_state.load t;
          toggle_gui = (fun () -> enable_gui_at_next_frame := true);
        }
    in
    G.set_save_state t.io.main_window (Save_state.save t);
    G.set_load_state t.io.main_window (Save_state.load t);
    let set_pixel = G.set_pixel t.io.main_window in
    let rec aux frame =
      if G.continue t.io.main_window then
        if G.shown t.io.main_window then (
          (* Stop emulation when GUI is displayed, and don't collect inputs *)
          G.render t.io.main_window;
          aux (frame + 1))
        else (
          (* Normal emulation *)
          if frame mod 100 = 0 then I.get_inputs t.state.input callbacks;
          NES.next_cycle t.state.cpu;
          A.next_cycle t.state.apu;
          P.next_cycle t.state.ppu set_pixel;
          P.next_cycle t.state.ppu set_pixel;
          P.next_cycle t.state.ppu set_pixel;
          (match P.should_render t.state.ppu with
          | None -> ()
          | Some bg_color ->
              A.output_frame t.state.apu;
              I.next_frame t.state.input;
              G.render_raw t.io.main_window;
              if !enable_gui_at_next_frame then (
                G.toggle_gui t.io.main_window ();
                enable_gui_at_next_frame := false)
              else G.clear t.io.main_window bg_color);
          P.Debug.render t.state.ppu t.io.debug;
          aux (frame + 1))
    in
    try aux 0
    with Common.End_of_movie -> Printf.printf "End of movie\n" (* end loop *)

  let close_io { io; state; _ } =
    A.exit state.apu;
    G.exit io.main_window
end

let input_backend movie record =
  match (movie, record) with
  | Some _, Some _ -> failwith "Cannot record while replaying movie"
  | Some path, None ->
      let module Movie = struct
        let file = path
      end in
      let module Movie_applied = Input_movie.Make_deter (Movie) in
      (module Movie_applied : Input.Backend)
  | None, Some path ->
      let module Out = struct
        let file = path
      end in
      let module Record_applied = Input_sdl.Make_record_deter (Out) in
      (module Record_applied)
  | None, None -> (module Input_sdl)

let make_apu headless =
  let backend =
    if headless then (module Apu.Dummy_backend : Apu.Backend)
    else (module Apu.Normal_backend)
  in
  let module Backend = (val backend) in
  let module Applied = Apu.Make (Backend) in
  (module Applied : Apu.S)

let make_gui headless disabled =
  let gui_func =
    if disabled then (module Gui.Disabled : Gui.SF) else (module Gui.Enabled)
  in
  let display =
    if headless then (module Display.Headless_backend : Display.S)
    else (module Display.Sdl_backend)
  in
  let module Gui_func = (val gui_func) in
  let module Display = (val display) in
  let module Applied = Gui_func (Display) in
  (module Applied : Gui.S)

let run filename movie record uncap_speed save_mp4 headless gui_disabled =
  let gui_disabled = if headless then true else gui_disabled in
  let cli_flags = { uncap_speed; save_mp4 } in
  let collector = C6502.IRQ_collector.create () in
  let nmi = C6502.NMI.create () in
  let rom = Rom.load filename in
  let apu_m = make_apu headless in
  let module Apu = (val apu_m : Apu.S) in
  let apu = Apu.create collector cli_flags in
  let input_backend = input_backend movie record in
  let module Input_backend = (val input_backend : Input.Backend) in
  let module Input = Input.Make (Input_backend) in
  let input = Input.create () in
  let mapper = Mapper.find rom in
  (* Create the CPU from the Mapper and ROM *)
  let module Mapper = (val mapper : Mapper.S) in
  let mapper = Mapper.create rom in
  let module Ppu = Ppu.Make (Mapper) in
  let ppu = Ppu.create mapper nmi in
  let module Memory_Map = Build_NES (Ppu) (Apu) (Mapper) (Input) in
  let module NES = C6502.Make (Memory_Map) in
  let gui_m = make_gui headless gui_disabled in
  let module Gui = (val gui_m : Gui.S) in
  let module System = Main (Ppu) (Gui) (Apu) (Mapper) (Input) (NES) in
  let state =
    System.create { mapper; rom; apu; ppu; input } collector nmi cli_flags
  in
  (try System.run state
   with C6502.Invalid_instruction (addr, opcode) ->
     Format.printf
       "The CPU encountered an invalid instruction %a at address %a.\n"
       C6502.Utils.pp_u8 opcode C6502.Utils.pp_u16 addr);
  System.close_io state

module Command_line = struct
  open Cmdliner

  let rom_arg =
    let doc = "Path to the ROM to run." in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"ROM_PATH" ~doc)

  let movie_arg =
    let doc = "Optional input file as outputed by --record to be replayed" in
    let i = Arg.info [ "m"; "movie" ] ~docv:"MOVIE_PATH" ~doc in
    Arg.(value & opt (some file) None & i)

  let record_arg =
    let doc = "Record input log to given file in a custom format" in
    let i = Arg.info [ "r"; "record" ] ~docv:"OUTPUT_PATH" ~doc in
    Arg.(value & opt (some string) None & i)

  let save_arg =
    let doc =
      "Save a (lossless) mp4 movie of the run to the given path (need ffmpeg \
       installed)"
    in
    let i = Arg.info [ "s"; "save" ] ~docv:"OUTPUT_PATH" ~doc in
    Arg.(value & opt (some string) None & i)

  let headless_arg =
    let doc = "Run in headless mode: no audio or video output" in
    let i = Arg.info [ "t"; "headless" ] ~doc in
    Arg.(value & flag i)

  let gui_arg =
    let doc = "Disable GUI" in
    let i = Arg.info [ "g"; "disable-gui" ] ~doc in
    Arg.(value & flag i)

  let speed_arg =
    let doc = "Uncap emulation speed" in
    let i = Arg.info [ "u"; "uncap" ] ~doc in
    Arg.(value & flag i)

  let run_term =
    Term.(
      const run $ rom_arg $ movie_arg $ record_arg $ speed_arg $ save_arg
      $ headless_arg $ gui_arg)

  let cmd =
    let doc = "experimental NES emulator written in OCaml" in
    let man =
      [
        `S Manpage.s_bugs;
        `P "File bug reports at https://github.com/Firobe/nes-ml";
      ]
    in
    let info = Cmd.info "nes-ml" ~doc ~man in
    Cmd.v info run_term

  let go () = exit (Cmd.eval cmd)
end

let () = Command_line.go ()
