exception Crash

let load_rom_memory rom =
  let open Rom_loader in
  Array.blit (Array.map C6502.Int_utils.u8 rom.chr_rom)
    0 Ppu.memory 0x0 (rom.config.chr_rom_size)

let rec ppu_exec_n_cycles n =
  if n > 0 then (
    Ppu.next_cycle ();
    ppu_exec_n_cycles (n - 1)
  )

let rec n_times f n =
  if n > 0 then (
    f (); n_times f (n - 1)
  )

let main_loop cpu limit =
  let module NesCpu = (val cpu : C6502.CPU) in
  let rec aux frame limit _sup_cycle =
    Input.get_inputs ();
    if frame <> limit && (Input.continue ()) then (
      (* NesCpu.print_state (); *)
      let old = !NesCpu.cycle_count in
      NesCpu.fetch_instr ();
      let elapsed = !NesCpu.cycle_count - old in
      (* n_times Apu.next_cycle ((elapsed + sup_cycle) / 2); *)
      n_times Ppu.next_cycle (elapsed * 3);
      aux (frame + 1) limit (elapsed mod 2)
    )
  in aux 0 limit 0

let main =
  let open Rom_loader in
  if Array.length Sys.argv > 1 then (
    let rom, pre_cpu = load_rom Sys.argv.(1) in
    (* Create the CPU from the Mapper and ROM *)
    let module NesCpu = C6502.MakeCPU ((val pre_cpu : MAPPER) (struct let get = rom end)) in
    load_rom_memory rom;
    Ppu.init NesCpu.interrupt rom.config.mirroring;
    NesCpu.Register.set `S 0xFDu ;
    NesCpu.Register.set `P 0x34u ;
    NesCpu.PC.init () ;
    NesCpu.enable_decimal := false ;
    (* Apu.init (); *)
    let cpu = (module NesCpu : C6502.CPU) in
    begin try
        main_loop cpu (-1) ;
      with C6502.Invalid_instruction (addr, opcode) ->
        Format.printf
          "The CPU encountered an invalid instruction %a at address %a.\n"
          C6502.Int_utils.pp_u8 opcode C6502.Int_utils.pp_u16 addr
    end ;
    (* Apu.exit (); *)
    Ppu.exit ()
  )
  else Printf.printf "No ROM provided\n"
