open Rom_loader

exception Crash

let load_rom_memory path =
    let rom = Rom_loader.load_rom path in
    let begin_address = 0x10000 - rom.config.prg_rom_size in
    Array.blit rom.prg_rom 0 Cpu.memory begin_address (rom.config.prg_rom_size)

let dump_memory () =
    let file = open_out_bin "memdump" in
    let store = Bytes.create 0x10000 in
    for i = 0 to (Array.length Cpu.memory) - 1 do
        Bytes.set store i @@ char_of_int Cpu.memory.(i)
    done ;
    output file store 0 (Bytes.length store) ;
    close_out file

let main =
    if Array.length Sys.argv > 1 then
        load_rom_memory Sys.argv.(1)
    else Printf.printf "No ROM provided\n"
    ;
    let count = ref 0 in
    let continue = ref true in
    Cpu.stack_pointer := 0xFD ;
    Cpu.processor_status := 0x34 ;
    Cpu.program_counter := (Cpu.memory.(0xFFFD) lsl 8) lor Cpu.memory.(0xFFFC) ;
    Cpu.program_counter := 0xC000 ;
    while !continue && Cpu.memory.(2) = 0 && Cpu.memory.(3) = 0 do
        count := !count + 1 ;
        let back = !Cpu.program_counter in
        (
        try
            Cpu.print_state ();
            Cpu.fetch_instr ()
        with _ -> dump_memory(); raise Crash
        )
        ;
        if back = !Cpu.program_counter then
            continue := false
    done ;
    Printf.printf "Errors %.2X %.2X\n" Cpu.memory.(2) Cpu.memory.(3);
    Printf.printf "END : trap encountered at %.4X\n%!" !Cpu.program_counter ;
    Printf.printf "Lasted %d cycles...\n%!" !count ;
    dump_memory ()
