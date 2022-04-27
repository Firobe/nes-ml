open Infix_int.Common
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
let is_in_ppu_range addr = addr >= 0x2000U && addr <= 0x2007U
let is_in_apu_range addr = addr >= 0x4000U && addr <= 0x4017U && addr <> 0x4014U
let is_in_cartridge_range addr = addr >= 0x8000U

module type Mapper = (C6502.MemoryMap with type input := rom)

type devices = {
  rom : rom;
  apu : Apu.t
}

module type Template = (C6502.MemoryMap with type input = devices)

module Memory_map_of_mapper (M : Mapper) : Template = struct
  type input = devices
  type t = {
    main : U8.t array;
    mapper : M.t;
    apu : Apu.t;
  }

  let create {rom; apu} = {
    main = Array.make 0x8000 0u;
    mapper = M.create rom;
    apu
  }

  let address_mirroring a =
    let open U16 in
    if a < 0x2000U then (* RAM mirroring *)
      a $& 0x07FFU
    else if (a $>> 13) = 1U then (* PPU mirroring *) (* TODO also mask ?*)
      a $& 0x2007U
    else a

  let read t (a : U16.t) : U8.t =
    let open U16 in
    let a = address_mirroring a in
    if is_in_ppu_range a then
      Ppu.get_register (to_int (logand a 7U))
    else if a = 0x4015U then
      Apu.read_register t.apu a
    else if a = 0x4016U then
      Input.next_register ()
    else if is_in_cartridge_range a then
      M.read t.mapper a
    else t.main.(?% a)

  let write t (a : U16.t) (v : U8.t) =
    let open U16 in
    let a = address_mirroring a in
    if is_in_ppu_range a then
      Ppu.set_register (to_int (logand a 7U)) v
    else if is_in_apu_range a then
      Apu.write_register t.apu v a
    else if a = 0x4014U then
      Ppu.dma (read t) (?$ v $<< 8)
    else if is_in_cartridge_range a then
      M.write t.mapper a v
    else t.main.(?% a) <- v
end

module NROM : Mapper = struct
  type t = U8.t array

  let create rom =
    let bank_nb = rom.config.prg_rom_size / 0x4000 in
    if bank_nb = 2 then Array.map C6502.Int_utils.u8 rom.prg_rom
    else
      let m = Array.make 0x8000 0x00 in (* 32K *)
      Array.blit rom.prg_rom 0 m 0 0x4000;
      Array.blit rom.prg_rom 0 m 0x4000 0x4000;
      Array.map C6502.Int_utils.u8 m

  open U16
  let read t a = t.(?% (a $& 0x7FFFU))
  let write t a v = t.(?% (a $& 0x7FFFU)) <- v
end

module UxROM : Mapper = struct
  type t = {
    banks : U8.t array array;
    mutable selected : int
  }

  let create rom =
    let bank_nb = rom.config.prg_rom_size / 0x4000 in
    let create_bank _ = Array.make 0x4000 0x00 in
    let banks = Array.init bank_nb create_bank in
    for i = 0 to bank_nb - 1 do
      Array.blit rom.prg_rom (0x4000 * i) banks.(i) 0 0x4000
    done ;
    let banks = Array.map (Array.map C6502.Int_utils.u8) banks in
    {banks; selected = 0}

  let last_bank t = t.banks.(Array.length t.banks - 1)

  open U16
  let read t a =
    if a >= 0xC000U then
      (last_bank t).(?% (a $& 0X3FFFU))
    else
      t.banks.(t.selected).(?% (a $& 0x3FFFU))

  let write t _ v = t.selected <- U8.to_int v
end

let mappers = [
  (0, (module NROM : Mapper));
  (2, (module UxROM))
]

let load_rom path =
  let rom = read_file path in
  let config = read_header rom in
  Printf.printf "Mapper %d\n" config.mapper_nb ;
  let memory_map = match List.assoc_opt config.mapper_nb mappers with
    | None -> raise (Invalid_ROM "Unsupported mapper")
    | Some x -> (module Memory_map_of_mapper(val x : Mapper) : Template)
  in
  Printf.printf "PRG ROM is %d bytes\n" config.prg_rom_size;
  Printf.printf "CHR ROM is %d bytes\n" config.chr_rom_size;
  Printf.printf "PRG RAM : %B\n" config.prg_ram_present;
  if config.tv_system then (
    raise (Invalid_ROM "PAL is not supported")
  );
  Printf.printf "TV system : %s\n"
    (if config.tv_system then "PAL" else "NTSC");
  Printf.printf "Mirroring type : %B\n%!" config.mirroring;
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
  }, memory_map
