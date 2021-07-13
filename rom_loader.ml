exception Invalid_ROM of string

type rom_config = {
  prg_rom_size : int;
  chr_rom_size : int;
  mirroring : bool;
  prg_ram_present : bool;
  trainer : bool;
  four_screen_vram : bool;
  vs_unisystem : bool;
  playchoice_10 : bool;
  nes_2_0 : bool;
  prg_ram_size : int;
  mapper_nb : int;
  tv_system : bool;
}

type rom = {
  config : rom_config;
  prg_rom : int array;
  chr_rom : int array;
  trainer : int array option;
}

module type ROM = sig
  val get : rom
end

let read_file path =
  let file = open_in_bin path in
  let size = in_channel_length file in
  let store = Bytes.create size in
  really_input file store 0 size ;
  let res = Array.make size 0 in
  for i = 0 to size - 1 do
    res.(i) <- int_of_char @@ Bytes.get store i
  done ;
  Printf.printf "Loaded %d bytes of memory\n" size ; res

let nth_bit b n =
  (b land (1 lsl n)) != 0

let read_header rom  =
  if rom.(0) != 0x4E || rom.(1) != 0x45 || rom.(2) != 0x53
     || rom.(3) != 0x1A then
    raise (Invalid_ROM "Wrong NES header")
  ;
  let config = {
    prg_rom_size = rom.(4) * 0x4000;
    chr_rom_size = rom.(5) * 0x2000;
    mirroring = nth_bit rom.(6) 0;
    prg_ram_present = nth_bit rom.(6) 1;
    trainer = nth_bit rom.(6) 2;
    four_screen_vram = nth_bit rom.(6) 3;
    vs_unisystem = nth_bit rom.(7) 0;
    playchoice_10 = nth_bit rom.(7) 1;
    nes_2_0 = rom.(7) land 0b1100 = 0b1000;
    prg_ram_size = rom.(8);
    mapper_nb = (rom.(6) lsr 4) lor (rom.(7) land 0xF0);
    tv_system = nth_bit rom.(9) 0;
  } in
  config

(* ################################ *)
(* ########### MAPPERS ############ *)
(* ################################ *)

(* Utils *)
let is_in_ppu_range addr = addr >= 0x2000 && addr <= 0x2007
let is_in_apu_range addr = addr >= 0x4000 && addr <= 0x4017 && addr <> 0x4014

module type MAPPER = functor (R : ROM) -> Cpu.Mmap

module Preload (C : Cpu.Mmap) = struct
  let address_mirroring a =
    if a < 0x2000 then (* RAM mirroring *)
      a land 0x07FF
    else if (a lsr 13) = 1 then (* PPU mirroring *)
      a land 0x2007
    else a

  let read a =
    let a = address_mirroring a in
    if is_in_ppu_range a then
      Ppu.get_register (a land 0x7)
    else if a = 0x4016 then
      Input.next_register ()
    else C.read a

  let write a v =
    let a = address_mirroring a in
    if is_in_ppu_range a then
      Ppu.set_register (a land 0x7) v
    else if is_in_apu_range a then
      Apu.write_register v a
    else if a = 0x4014 then
      Ppu.dma read (v lsl 8)
    else C.write a v
end

module NROM (R : ROM) = Preload (struct
    let mem =
      let m = Array.make 0x10000 0x00 in
      let rom = R.get in
      let begin_address = 0x10000 - rom.config.prg_rom_size in
      Array.blit rom.prg_rom 0 m begin_address (rom.config.prg_rom_size); m

    let read a = mem.(a)
    let write a v = mem.(a) <- v
  end)

module UxROM (R : ROM) = Preload (struct
    let mem = Array.make 0x8000 0x00 (* main memory *)

    let banks =
      let rom = R.get in
      let bank_nb = rom.config.prg_rom_size / 0x4000 in
      let create_bank _ = Array.make 0x4000 0x00 in
      let banks = Array.init bank_nb create_bank in
      for i = 0 to bank_nb - 1 do
        Array.blit rom.prg_rom (0x4000 * i) banks.(i) 0 0x4000
      done ; banks

    let last_bank = banks.(Array.length banks - 1)
    let selected = ref 0

    let read a =
      if a >= 0xC000 then
        last_bank.(a land 0x3FFF)
      else if a >= 0x8000 then
        banks.(!selected).(a land 0x3FFF)
      else mem.(a)

    let write a v =
      if a >= 0x8000 then (
        selected := v
      )
      else mem.(a) <- v
  end)

let mappers = [
  (0, (module NROM : MAPPER));
  (2, (module UxROM))
]

let load_rom path =
  let rom = read_file path in
  let config = read_header rom in
  Printf.printf "Mapper %d\n" config.mapper_nb ;
  let mapper = match List.assoc_opt config.mapper_nb mappers with
    | None -> raise (Invalid_ROM "Unsupported mapper")
    | Some x -> x
  in
  Printf.printf "PRG ROM is %d bytes\n" config.prg_rom_size;
  Printf.printf "CHR ROM is %d bytes\n" config.chr_rom_size;
  Printf.printf "PRG RAM : %B\n" config.prg_ram_present;
  Printf.printf "TV system : %s\n"
    (if config.tv_system then "PAL" else "NTSC");
  Printf.printf "Mirroring type : %B\n" config.mirroring;
  let cur_address = ref 0x10 in
  let trainer = if not config.trainer then None else (
      cur_address := !cur_address + 0x200;
      Some (Array.sub rom 0x10 512)
    ) in
  let prg_rom = Array.sub rom !cur_address config.prg_rom_size in
  cur_address := !cur_address + config.prg_rom_size ;
  let chr_rom = Array.sub rom !cur_address config.chr_rom_size in
  cur_address := !cur_address + config.chr_rom_size;
  (* ignored PRG_RAM, Playchoices data, title *)
  {
    config = config;
    prg_rom = prg_rom;
    chr_rom = chr_rom;
    trainer = trainer;
  }, mapper
