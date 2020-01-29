open Rom_loader

module type ROM = sig
  val get : rom
end

module Mapper0 (R : ROM) = struct
  let is_in_ppu_range addr = (addr lsr 13) = 1
  let is_in_apu_range addr = addr >= 0x4000 && addr <= 0x4017
  let mem =
    let m = Array.make 0x10000 0x00 in
    let rom = R.get in
    let begin_address = 0x10000 - rom.config.prg_rom_size in
    Array.blit rom.prg_rom 0 m begin_address (rom.config.prg_rom_size); m

  let read a =
    if is_in_ppu_range a then
      Ppu.get_register a
    else if a = 0x4016 then
      Input.next_register ()
    else mem.(a)
  let write a v =
    if is_in_ppu_range a then
      Ppu.set_register a v
    else if a = 0x4014 then
      Ppu.dma mem (v lsl 8)
    else if is_in_apu_range a then
      Apu.write_register v a
    else mem.(a) <- v
end

exception Crash

let load_rom_memory rom =
  Array.blit rom.chr_rom 0 Ppu.memory 0x0 (rom.config.chr_rom_size)

let rec ppu_exec_n_cycles n =
  if n > 0 then (
    Ppu.next_cycle ();
    ppu_exec_n_cycles (n - 1)
  )

let rec n_times f n =
  if n > 0 then (
    f (); n_times f (n - 1)
  )

let rec main_loop frame limit sup_cycle cpu =
  Input.get_inputs ();
  if frame != limit && (Input.continue ()) then (
    (*         NesCpu.print_state (); *)
    let module NesCpu = (val cpu : Cpu.Full) in
    let old = !NesCpu.cycle_count in
    NesCpu.fetch_instr ();
    let elapsed = !NesCpu.cycle_count - old in
    n_times Apu.next_cycle ((elapsed + sup_cycle) / 2);
    n_times Ppu.next_cycle (elapsed * 3);
    main_loop (frame + 1) limit (elapsed mod 2) cpu
  )

let start_main_loop lim = main_loop 0 lim 0

let main =
  if Array.length Sys.argv > 1 then (
    let rom = Rom_loader.load_rom Sys.argv.(1) in
    let module NesCpu = Cpu.Make (Mapper0 (struct let get = rom end)) in
    load_rom_memory rom;
    Ppu.init NesCpu.interrupt rom.config.mirroring;
    NesCpu.stack_pointer := 0xFD ;
    NesCpu.processor_status := 0x34 ;
    NesCpu.program_counter := (NesCpu.M.read (0xFFFD) lsl 8) lor NesCpu.M.read (0xFFFC) ;
    Apu.init ();
    let cpu = (module NesCpu : Cpu.Full) in
    start_main_loop (-1) cpu;
    Apu.exit ();
    Ppu.exit ()
  )
  else Printf.printf "No ROM provided\n"
