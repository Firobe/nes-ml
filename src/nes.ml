let load_rom_memory rom =
  let open Rom in
  Array.blit (Array.map C6502.Int_utils.u8 rom.chr_rom)
    0 Ppu.State.Mem.main 0x0 (rom.config.chr_rom_size)

let rec n_times f n =
  if n > 0 then (
    f (); n_times f (n - 1)
  )

type disps = {
  main : Display.t;
  debug : (Ppu.Debug.t option) ref
}

module FPS = struct
  let last_frame = ref 0
  let next_time = ref 0.
  let check () =
    let now = Unix.time () in
    if now >= !next_time then (
      let new_frame = !Ppu.State.Rendering.frame in
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
  apu : Apu.t
}

module Build_NES (M : Mapper.S) : (C6502.MemoryMap with type input = devices) = struct
  open Infix_int.Common
  type input = devices
  type t = {
    main : U8.t array;
    mapper : M.t;
    apu : Apu.t;
  }

  let create {rom; apu} = {
    main = Array.make 0x8000 0u;
    mapper = M.create rom;
    apu
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
      Ppu.get_register (to_int (logand a 7U))
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
      Ppu.set_register (to_int (logand a 7U)) v
    else if is_in_apu_range a then
      Apu.write_register t.apu v a
    else if a = 0x4014U then
      Ppu.dma (read t) (?$ v $<< 8)
    else if is_in_cartridge_range a then
      M.write t.mapper a v
    else t.main.(?% a) <- v
end


let () =
  if Array.length Sys.argv <= 1 then (
    Printf.printf "No ROM provided\n"
  ) else (
    let apu = Apu.create () in
    let rom = Rom.load Sys.argv.(1) in
    let mapper = Mapper.find rom in
    (* Create the CPU from the Mapper and ROM *)
    let module Mapper = (val mapper : Mapper.S) in
    let module Memory_Map = Build_NES(Mapper) in
    let module NES = C6502.Make (Memory_Map) in
    let cpu = NES.create {apu; rom} in
    load_rom_memory rom;
    let main = Ppu.init (fun () -> NES.interrupt cpu) rom.config.mirroring in
    let disps = {
      debug = ref None;
      main
    } in
    NES.Register.set (NES.registers cpu) `S 0xFDu ;
    NES.Register.set (NES.registers cpu) `P 0x34u ;
    NES.PC.init (NES.pc cpu) (NES.memory cpu) ;
    NES.enable_decimal cpu false ;
    begin try
        let rec aux frame =
          if frame mod 100 = 0 then (Input.get_inputs ());
          if Input.continue () then (
            if Input.(key_pressed Debug_on) && !(disps.debug) = None then (
              disps.debug := Some (Ppu.Debug.init ())
            )
            else if Input.(key_pressed Debug_off) then (
              match !(disps.debug) with
              | Some d -> Ppu.Debug.delete d; disps.debug := None
              | None -> ()
            );
            FPS.check ();
            let old = NES.cycle_count cpu in
            NES.fetch_instr cpu;
            let elapsed = NES.cycle_count cpu - old in
            n_times (fun () -> Apu.next_cycle apu) elapsed;
            n_times (fun () -> Ppu.next_cycle disps.main) (elapsed * 3);
            Ppu.Debug.render !(disps.debug);
            aux (frame + 1)
          )
        in aux 0
      with C6502.Invalid_instruction (addr, opcode) ->
        Format.printf
          "The CPU encountered an invalid instruction %a at address %a.\n"
          C6502.Int_utils.pp_u8 opcode C6502.Int_utils.pp_u16 addr
    end ;
    Apu.exit apu;
    Ppu.exit main
  )
