open Rom_loader

exception Crash

let dump_all_memory () =
    Cpu.dump_memory ();
    Ppu.dump_memory ()

let load_rom_memory rom =
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

let scale = 4

let rec main_loop frame limit =
    Input.get_inputs ();
    if frame != limit && (Input.continue ()) then (
        Ppu.render ();
        Ppu.debug_vram scale;
        if !Ppu.nmi_enabled then (
            Cpu.interrupt ()
        ) ;
        cpu_exec_n_cycles 29780;
        main_loop (frame + 1) limit
    )

let start_main_loop = main_loop 0


let main =
    Graphics.open_graph "";
    Graphics.resize_window (256 * scale) (128 * scale);
    Graphics.auto_synchronize false;
    if Array.length Sys.argv > 1 then (
        let rom = Rom_loader.load_rom Sys.argv.(1) in
        load_rom_memory rom;
        Ppu.init rom.config.mirroring;
        Cpu.stack_pointer := 0xFD ;
        Cpu.processor_status := 0x34 ;
        Cpu.program_counter := (Cpu.memory.(0xFFFD) lsl 8) lor Cpu.memory.(0xFFFC) ;
        start_main_loop (-1);
        Ppu.exit ()
    )
    else Printf.printf "No ROM provided\n"
