let load_rom_memory ppu rom =
  let open Rom in
  Ppu.init_memory ppu (Array.map C6502.Int_utils.u8 rom.chr_rom)
    rom.config.chr_rom_size

let rec n_times f n =
  if n > 0 then (
    f (); n_times f (n - 1)
  )

module FPS = struct
  let last_frame = ref 0
  let next_time = ref 0.
  let check ppu =
    let now = Unix.time () in
    if now >= !next_time then (
      let new_frame = Ppu.frame ppu in
      let diff = new_frame - !last_frame in
      if diff < 60 then (
        Printf.printf "Too slow! %d FPS\n%!" diff;
      );
      last_frame := new_frame;
      next_time := now +. 1.
    )
end

type devices = {
  rom : Rom.t;
  apu : Apu.t;
  ppu : Ppu.t
}

module Build_NES (M : Mapper.S) : (C6502.MemoryMap with type input = devices) = struct
  open Infix_int.Common
  type input = devices
  type t = {
    main : U8.t array;
    mapper : M.t;
    apu : Apu.t;
    ppu : Ppu.t
  }

  let create {rom; apu; ppu} = {
    main = Array.make 0x8000 0u;
    mapper = M.create rom;
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
      Input.next_register ()
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
  }
  type io = {
    mutable debug : Ppu.Debug.t option;
    main : Display.t;
  }
  type t = {
    mutable state : state;
    mutable saved_state : state;
    io : io
  }

  let deep_copy (type t) (x : t) : t = 
    let buf = Marshal.(to_bytes x [Closures]) in
    Marshal.from_bytes buf 0

  let save_state t =
    t.saved_state <- deep_copy t.state

  let load_state t =
    t.state <- deep_copy t.saved_state

  let create ({apu; rom; ppu} : devices) =
    let cpu = NES.create {apu; rom; ppu} in
    Ppu.set_interrupt ppu (fun () -> NES.interrupt cpu);
    load_rom_memory ppu rom;
    NES.Register.set (NES.registers cpu) `S 0xFDu ;
    NES.Register.set (NES.registers cpu) `P 0x34u ;
    NES.PC.init (NES.pc cpu) (NES.memory cpu) ;
    NES.enable_decimal cpu false ;
    let state = {cpu; apu; ppu; rom} in
    let saved_state = deep_copy state in
    let io = {
      debug = None;
      main = Ppu.Window.create ()
    } in {state; saved_state; io}

  let manage_debug_windows t =
    if Input.(key_pressed Debug_on) && t.io.debug = None then (
      t.io.debug <- Some (Ppu.Debug.create ())
    )
    else if Input.(key_pressed Debug_off) then (
      match t.io.debug with
      | Some d -> Ppu.Debug.delete d; t.io.debug <- None
      | None -> ()
    )

  let manage_save_states t =
    if Input.(key_pressed Save_state) then save_state t
    else if Input.(key_pressed Load_state) then load_state t

  let run t =
    let rec aux frame =
      if frame mod 100 = 0 then (Input.get_inputs ());
      if Input.continue () then (
        manage_debug_windows t;
        manage_save_states t;
        FPS.check t.state.ppu;
        let old = NES.cycle_count t.state.cpu in
        NES.fetch_instr t.state.cpu;
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
    let rom = Rom.load Sys.argv.(1) in
    let apu = Apu.create () in
    let ppu = Ppu.create rom.config.mirroring in
    let mapper = Mapper.find rom in
    (* Create the CPU from the Mapper and ROM *)
    let module Mapper = (val mapper : Mapper.S) in
    let module Memory_Map = Build_NES(Mapper) in
    let module NES = C6502.Make (Memory_Map) in
    let module System = Main(NES) in
    let state = System.create {rom; apu; ppu} in
    begin try
        System.run state
      with C6502.Invalid_instruction (addr, opcode) ->
        Format.printf
          "The CPU encountered an invalid instruction %a at address %a.\n"
          C6502.Int_utils.pp_u8 opcode C6502.Int_utils.pp_u16 addr
    end ;
    System.close_io state
  )
