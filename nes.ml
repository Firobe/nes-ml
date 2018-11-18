open Rom_loader

exception Crash

let dump_all_memory () =
    Cpu.dump_memory ();
    Ppu.dump_memory ()

let load_rom_memory path =
    let rom = Rom_loader.load_rom path in
    let begin_address = 0x10000 - rom.config.prg_rom_size in
    Array.blit rom.prg_rom 0 Cpu.memory begin_address (rom.config.prg_rom_size);
    Array.blit rom.chr_rom 0 Ppu.memory 0x0 (rom.config.chr_rom_size)

let rec cpu_exec_n_cycles n = 
    if n > 0 then (
        let old = !Cpu.cycle_count in
        Cpu.fetch_instr ();
        let elapsed = !Cpu.cycle_count - old in
        cpu_exec_n_cycles (n - elapsed)
    )

let rec main_loop frame limit =
    Input.get_inputs ();
    if frame != limit && (Input.continue ()) then (
        Ppu.render ();
        if !Ppu.nmi_enabled then (
            Cpu.interrupt ()
        ) ;
        cpu_exec_n_cycles 29780;
        main_loop (frame + 1) limit
    )

let start_main_loop = main_loop 0

let main =
    Ppu.init ();
    if Array.length Sys.argv > 1 then
        load_rom_memory Sys.argv.(1)
    else Printf.printf "No ROM provided\n"
    ;
    Cpu.stack_pointer := 0xFD ;
    Cpu.processor_status := 0x34 ;
    Cpu.program_counter := (Cpu.memory.(0xFFFD) lsl 8) lor Cpu.memory.(0xFFFC) ;
    start_main_loop (-1);
    Ppu.exit ()
