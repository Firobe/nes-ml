open Rom_loader

module NesCpu = Cpu.Make (struct
    let is_in_ppu_range addr = (addr lsr 13) = 1
    let read mem a =
        if is_in_ppu_range a then
            Ppu.get_register a
        else if a = 0x4016 then
            Input.next_register ()
        else mem.(a)
    let write mem a v =
        if is_in_ppu_range a then
            Ppu.set_register a v
        else if a = 0x4014 then
            Ppu.dma mem (v lsl 8)
        else mem.(a) <- v
end)

exception Crash

let load_rom_memory rom =
    let begin_address = 0x10000 - rom.config.prg_rom_size in
    Array.blit rom.prg_rom 0 NesCpu.memory begin_address (rom.config.prg_rom_size);
    Array.blit rom.chr_rom 0 Ppu.memory 0x0 (rom.config.chr_rom_size)

let rec cpu_exec_n_cycles n = 
    if n > 0 then (
        let old = !NesCpu.cycle_count in
        NesCpu.fetch_instr ();
        let elapsed = !NesCpu.cycle_count - old in
        cpu_exec_n_cycles (n - elapsed)
    )

let rec main_loop frame limit =
    Input.get_inputs ();
    if frame != limit && (Input.continue ()) then (
        Ppu.render ();
        cpu_exec_n_cycles 29780;
        main_loop (frame + 1) limit
    )

let start_main_loop = main_loop 0

let main =
    if Array.length Sys.argv > 1 then (
        let rom = Rom_loader.load_rom Sys.argv.(1) in
        load_rom_memory rom;
        Ppu.init NesCpu.interrupt rom.config.mirroring;
        NesCpu.stack_pointer := 0xFD ;
        NesCpu.processor_status := 0x34 ;
        NesCpu.program_counter := (NesCpu.memory.(0xFFFD) lsl 8) lor NesCpu.memory.(0xFFFC) ;
        start_main_loop (-1);
        Ppu.exit ()
    )
    else Printf.printf "No ROM provided\n"
