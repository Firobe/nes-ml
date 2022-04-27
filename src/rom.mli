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

type t = {
  config : rom_config;
  prg_rom : int array;
  chr_rom : int array;
  trainer : int array option;
}

val load : string -> t
