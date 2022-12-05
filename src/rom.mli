(** Read binary iNES ROMS from files *)

exception Invalid_ROM of string
(** Thrown if the given ROM is either ill-formed or unsupported. *)

type rom_config = {
  prg_rom_size : int;
  chr_rom_size : int;
  mirroring : bool;
  prg_ram_present : bool;
  trainer : bool;
  four_screen_vram : bool;
  vs_unisystem : bool;
  playchoice_10 : bool;
  prg_ram_size : int;
  mapper_nb : int;
  tv_system : [ `NTSC | `PAL ];
}
(** The various iNES header attributes *)

type t = {
  file_name : string;
  hash : string;  (** Truncated MD5 hash of the ROM payload *)
  config : rom_config;
  prg_rom : int array;
  chr_rom : int array;
  trainer : int array option;
}

val load : string -> t
(** Load a ROM from a file path *)

(** Save states filename generation, with three slots *)
module Save_file : sig
  type slot = S1 | S2 | S3

  val make_name : t -> slot -> string
  val find_matching_name : t -> slot -> string option
end
