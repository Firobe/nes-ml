open Rom_loader

module SCpu = Cpu.Make (struct
    let is_in_ppu_range addr = (addr lsr 13) = 1

    let read mem a =
        let ba = a land 0xFFFF in
        if is_in_ppu_range ba then
            Ppu.get_register ba
        else if ba = 0x4016 then
            Input.next_register ()
        else mem.(ba)

    let write mem a v =
        let ba = a land 0xFFFF in
        if is_in_ppu_range ba then
            Ppu.set_register ba v
        else if ba = 0x4014 then (
            let cpu_begin = v lsl 8 in
            Array.blit mem cpu_begin Ppu.oam
                !Ppu.oam_address 0x100
        )
        else mem.(ba) <- v
end)

exception Crash

let load_rom_memory rom =
    let begin_address = 0x10000 - rom.config.prg_rom_size in
    Array.blit rom.prg_rom 0 SCpu.memory begin_address (rom.config.prg_rom_size);
    Array.blit rom.chr_rom 0 Ppu.memory 0x0 (rom.config.chr_rom_size)

let rec cpu_exec_n_cycles n = 
    if n > 0 then (
        let old = !SCpu.cycle_count in
        SCpu.fetch_instr ();
        let elapsed = !SCpu.cycle_count - old in
        cpu_exec_n_cycles (n - elapsed)
    )

let rec main_loop frame limit =
    Input.get_inputs ();
    if frame != limit && (Input.continue ()) then (
        Ppu.render ();
        if !Ppu.nmi_enabled then (
            SCpu.interrupt ()
        ) ;
        cpu_exec_n_cycles 29780;
        main_loop (frame + 1) limit
    )

let start_main_loop = main_loop 0

let main =
    if Array.length Sys.argv > 1 then (
        let rom = Rom_loader.load_rom Sys.argv.(1) in
        load_rom_memory rom;
        Ppu.init rom.config.mirroring;
        SCpu.stack_pointer := 0xFD ;
        SCpu.processor_status := 0x34 ;
        SCpu.program_counter := (SCpu.memory.(0xFFFD) lsl 8) lor SCpu.memory.(0xFFFC) ;
        start_main_loop (-1);
        Ppu.exit ()
    )
    else Printf.printf "No ROM provided\n"
