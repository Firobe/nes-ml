open Rom_loader

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

let rec main_loop frame limit _sup_cycle cpu =
  Input.get_inputs ();
  if frame != limit && (Input.continue ()) then (
    (*         NesCpu.print_state (); *)
    let module NesCpu = (val cpu : Cpu.Full) in
    let old = !NesCpu.cycle_count in
    NesCpu.fetch_instr ();
    let elapsed = !NesCpu.cycle_count - old in
    (* n_times Apu.next_cycle ((elapsed + sup_cycle) / 2); *)
    n_times Ppu.next_cycle (elapsed * 3);
    main_loop (frame + 1) limit (elapsed mod 2) cpu
  )

let start_main_loop lim = main_loop 0 lim 0

let main =
  if Array.length Sys.argv > 1 then (
    let rom, mapper = Rom_loader.load_rom Sys.argv.(1) in
    (* Create the CPU from the Mapper and ROM *)
    let module NesCpu = Cpu.Make ((val mapper : MAPPER) (struct let get = rom end)) in
    load_rom_memory rom;
    Ppu.init NesCpu.interrupt rom.config.mirroring;
    NesCpu.Register.set `S 0xFD ;
    NesCpu.Register.set `P 0x34 ;
    NesCpu.Register.set `PC @@ (NesCpu.M.read (0xFFFD) lsl 8) lor NesCpu.M.read (0xFFFC) ;
    (* Apu.init (); *)
    let cpu = (module NesCpu : Cpu.Full) in
    start_main_loop (-1) cpu;
    (* Apu.exit (); *)
    Ppu.exit ()
  )
  else Printf.printf "No ROM provided\n"
