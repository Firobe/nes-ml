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

let main_loop disps cpu apu limit =
  let module NesCpu = (val cpu : C6502.CPU) in
  let module NesApu = (val apu : Apu.APU) in
  let rec aux frame limit =
    if frame mod 100 = 0 then (Input.get_inputs ());
    if frame <> limit && (Input.continue ()) then (
      if Input.(key_pressed Debug_on) && !(disps.debug) = None then (
        disps.debug := Some (Ppu.Debug.init ())
      )
      else if Input.(key_pressed Debug_off) then (
        match !(disps.debug) with
        | Some d -> Ppu.Debug.delete d; disps.debug := None
        | None -> ()
      );
      FPS.check ();
      let old = !NesCpu.cycle_count in
      NesCpu.fetch_instr ();
      let elapsed = !NesCpu.cycle_count - old in
      n_times NesApu.next_cycle elapsed;
      n_times (fun () -> Ppu.next_cycle disps.main) (elapsed * 3);
      Ppu.Debug.render !(disps.debug);
      aux (frame + 1) limit
    )
  in aux 0 limit;
  NesApu.exit ()

let () =
  let open Rom_loader in
  if Array.length Sys.argv > 1 then (
    let apu = Apu.init () in
    let rom, pre_cpu = load_rom apu Sys.argv.(1) in
    (* Create the CPU from the Mapper and ROM *)
    let module NesCpu = C6502.MakeCPU ((val pre_cpu : MAPPER) (struct let get = rom end)) in
    load_rom_memory rom;
    let main = Ppu.init NesCpu.interrupt rom.config.mirroring in
    let disps = {
      debug = ref None;
      main
    } in
    NesCpu.Register.set `S 0xFDu ;
    NesCpu.Register.set `P 0x34u ;
    NesCpu.PC.init () ;
    NesCpu.enable_decimal := false ;
    let cpu = (module NesCpu : C6502.CPU) in
    begin try
        main_loop disps cpu apu (-1) ;
      with C6502.Invalid_instruction (addr, opcode) ->
        Format.printf
          "The CPU encountered an invalid instruction %a at address %a.\n"
          C6502.Int_utils.pp_u8 opcode C6502.Int_utils.pp_u16 addr
    end ;
    Ppu.exit main
  )
  else Printf.printf "No ROM provided\n"
