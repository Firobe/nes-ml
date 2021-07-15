open Rom_loader
open Cpu.Int_utils

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

let main_loop cpu limit =
  let module NesCpu = (val cpu : Cpu.Full) in
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
  if Array.length Sys.argv > 1 then (
    let rom, pre_cpu = Rom_loader.load_rom Sys.argv.(1) in
    (* Create the CPU from the Mapper and ROM *)
    let module NesCpu = Cpu.Make ((val pre_cpu : MAPPER) (struct let get = rom end)) in
    load_rom_memory rom;
    Ppu.init NesCpu.interrupt rom.config.mirroring;
    NesCpu.Register.set `S (u8 0xFD) ;
    NesCpu.Register.set `P (u8 0x34) ;
    NesCpu.PC.init () ;
    (* Apu.init (); *)
    let cpu = (module NesCpu : Cpu.Full) in
    begin try
        main_loop cpu (-1) ;
      with Cpu.Invalid_instruction (addr, opcode) ->
        Format.printf
          "The CPU encountered an invalid instruction %a at address %a.\n"
          pp_u8 opcode pp_u16 addr
    end ;
    (* Apu.exit (); *)
    Ppu.exit ()
  )
  else Printf.printf "No ROM provided\n"
