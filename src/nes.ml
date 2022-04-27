let load_rom_memory rom =
  let open Rom_loader in
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

let () =
  let open Rom_loader in
  if Array.length Sys.argv <= 1 then (
    Printf.printf "No ROM provided\n"
  ) else (
    let apu = Apu.create () in
    let rom, memory_map_m = load_rom Sys.argv.(1) in
    (* Create the CPU from the Mapper and ROM *)
    let module MMap = (val memory_map_m : Template) in
    let module NES = C6502.Make (MMap) in
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
