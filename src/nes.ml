let load_rom_memory ppu rom =
  let open Rom in
  Ppu.init_memory ppu (Array.map C6502.Int_utils.u8 rom.chr_rom)
    rom.config.chr_rom_size

let rec n_times f n =
  if n > 0 then (
    f (); n_times f (n - 1)
  )

type devices = {
  rom : Rom.t;
  apu : Apu.t;
  ppu : Ppu.t;
}

module Build_NES (M : Mapper.S) : (C6502.MemoryMap with type input = devices) = struct
  open Infix_int.Common
  type input = devices
  type t = {
    main : U8.t array;
    mapper : M.t;
    apu : Apu.t;
    ppu : Ppu.t;
    input : Input.t;
  }

  let create {rom; apu; ppu} = {
    main = Array.make 0x8000 0u;
    mapper = M.create rom;
    input = Input.create ();
    apu; ppu
  }

  (* Utils *)
  let is_in_ppu_range addr = addr >= 0x2000U && addr <= 0x2007U
  let is_in_apu_range addr = addr >= 0x4000U && addr <= 0x4017U && addr <> 0x4014U
  let is_in_cartridge_range addr = addr >= 0x8000U


  let address_mirroring a =
    let open U16 in
    if a < 0x2000U then (* RAM mirroring *)
      a $& 0x07FFU
    else if (a $>> 13) = 1U then (* PPU mirroring *) (* TODO also mask ?*)
      a $& 0x2007U
    else a

  let read t (a : U16.t) : U8.t =
    let open U16 in
    let a = address_mirroring a in
    if is_in_ppu_range a then
      Ppu.get_register t.ppu (to_int (logand a 7U))
    else if a = 0x4015U then
      Apu.read_register t.apu a
    else if a = 0x4016U then
      Input.next_register t.input
    else if is_in_cartridge_range a then
      M.read t.mapper a
    else t.main.(?% a)

  let write t (a : U16.t) (v : U8.t) =
    let open U16 in
    let a = address_mirroring a in
    if is_in_ppu_range a then
      Ppu.set_register t.ppu (to_int (logand a 7U)) v
    else if is_in_apu_range a then
      Apu.write_register t.apu v a
    else if a = 0x4014U then
      Ppu.dma t.ppu (read t) (?$ v $<< 8)
    else if is_in_cartridge_range a then
      M.write t.mapper a v
    else t.main.(?% a) <- v
end

module Main (NES : (C6502.CPU with type input := devices)) = struct
  type state = {
    cpu : NES.t;
    apu : Apu.t;
    ppu : Ppu.t;
    rom : Rom.t;
    collector : C6502.IRQ_collector.t;
  }
  type io = {
    mutable debug : Ppu.Debug.t option;
    main : Display.t;
  }
  type t = {
    mutable state : state;
    io : io
  }

  module Save_state = struct
    let save t slot =
      try
        let save_name = Rom.Save_file.make_name t.state.rom slot in
        let chan = open_out_bin save_name in
        Marshal.(to_channel chan t.state [Closures]);
        close_out chan
      with
      | Sys_error err -> Printf.printf "Cannot save state: %s\n%!" err

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
          "Cannot parse save state (%s). It was \
           probably saved with a different version of the emulator.\n%!" msg
  end

  let create ({apu; rom; ppu} : devices) collector =
    let cpu = NES.create ~collector {apu; rom; ppu} in
    Ppu.set_interrupt ppu (fun () -> NES.nmi cpu);
    load_rom_memory ppu rom;
    NES.Register.set (NES.registers cpu) `S 0xFDu ;
    NES.Register.set (NES.registers cpu) `P 0x34u ;
    NES.PC.init (NES.pc cpu) (NES.memory cpu) ;
    NES.enable_decimal cpu false ;
    let state = {cpu; apu; ppu; rom; collector} in
    let io = {
      debug = None;
      main = Ppu.Window.create ()
    } in {state; io}

  let debug_callback t () =
    match t.io.debug with
    | None ->
      t.io.debug <- Some (Ppu.Debug.create ())
    | Some d ->
      Ppu.Debug.delete d; t.io.debug <- None

  let run t =
    let callbacks = Input.{
      debug = debug_callback t;
      save_state = Save_state.save t;
      load_state = Save_state.load t;
    } in
    let rec aux frame =
      if frame mod 100 = 0 then (Input.get_inputs callbacks);
      if Input.continue () then (
        let old = NES.cycle_count t.state.cpu in
        NES.next_cycle t.state.cpu;
        let elapsed = NES.cycle_count t.state.cpu - old in
        n_times (fun () -> Apu.next_cycle t.state.apu) elapsed;
        n_times (fun () -> Ppu.next_cycle t.state.ppu t.io.main) (elapsed * 3);
        Ppu.Debug.render t.state.ppu t.io.debug;
        aux (frame + 1)
      )
    in aux 0

  let close_io {io; state; _} =
    Apu.exit state.apu;
    Ppu.Window.exit io.main
end


let () =
  if Array.length Sys.argv <= 1 then (
    Printf.printf "No ROM provided\n"
  ) else (
    let collector = C6502.IRQ_collector.create () in
    let rom = Rom.load Sys.argv.(1) in
    let apu = Apu.create collector in
    let ppu = Ppu.create rom.config.mirroring in
    let mapper = Mapper.find rom in
    (* Create the CPU from the Mapper and ROM *)
    let module Mapper = (val mapper : Mapper.S) in
    let module Memory_Map = Build_NES(Mapper) in
    let module NES = C6502.Make (Memory_Map) in
    let module System = Main(NES) in
    let state = System.create {rom; apu; ppu} collector in
    begin try
        System.run state
      with C6502.Invalid_instruction (addr, opcode) ->
        Format.printf
          "The CPU encountered an invalid instruction %a at address %a.\n"
          C6502.Int_utils.pp_u8 opcode C6502.Int_utils.pp_u16 addr
    end ;
    System.close_io state
  )
