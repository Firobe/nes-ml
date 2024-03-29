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
  prg_ram_size : int;
  mapper_nb : int;
  tv_system : [ `NTSC | `PAL ];
}

type t = {
  file_name : string;
  hash : string;
  config : rom_config;
  prg_rom : int array;
  chr_rom : int array;
  trainer : int array option;
}

let truncated_hash b =
  let trunc_size = 6 in
  let open Digestif.MD5 in
  let hash = digest_bytes b in
  let hex = to_hex hash in
  String.sub hex 0 trunc_size

let read_file path =
  let file = open_in_bin path in
  let size = in_channel_length file in
  let store = Bytes.create size in
  really_input file store 0 size;
  let res = Array.make size 0 in
  for i = 0 to size - 1 do
    res.(i) <- int_of_char @@ Bytes.get store i
  done;
  let hash = truncated_hash store in
  Printf.printf "Loaded %d bytes of memory (%s)\n" size hash;
  (res, hash)

module Save_file = struct
  type slot = S1 | S2 | S3

  let suffix = ".sav"
  let separator = "_"
  let num_of_slot = function S1 -> 1 | S2 -> 2 | S3 -> 3

  let make_name t slot =
    t.hash ^ separator ^ t.file_name ^ suffix ^ Int.to_string (num_of_slot slot)

  let find_matching_name t slot =
    let cwd = Sys.getcwd () in
    let files = Sys.readdir cwd in
    let sn = num_of_slot slot in
    let regex =
      Printf.sprintf "%s%s.*\\%s%d$" t.hash separator suffix sn |> Str.regexp
    in
    let pred candidate = Str.string_match regex candidate 0 in
    Array.find_opt pred files
end

let nth_bit b n = b land (1 lsl n) != 0

let read_header rom =
  if rom.(0) != 0x4E || rom.(1) != 0x45 || rom.(2) != 0x53 || rom.(3) != 0x1A
  then raise (Invalid_ROM "Wrong NES header");
  let nes2 = rom.(7) land 0b1100 = 0b1000 in
  if nes2 then raise (Invalid_ROM "NES 2.0 ROM format not supported")
  else
    {
      prg_rom_size = rom.(4) * 0x4000;
      chr_rom_size = rom.(5) * 0x2000;
      mirroring = nth_bit rom.(6) 0;
      prg_ram_present = nth_bit rom.(6) 1;
      trainer = nth_bit rom.(6) 2;
      four_screen_vram = nth_bit rom.(6) 3;
      vs_unisystem = nth_bit rom.(7) 0;
      playchoice_10 = nth_bit rom.(7) 1;
      prg_ram_size = rom.(8);
      mapper_nb = (rom.(6) lsr 4) lor (rom.(7) land 0xF0);
      tv_system = (if nth_bit rom.(9) 0 then `PAL else `NTSC);
    }

let load path =
  let file_name =
    match Fpath.of_string path with
    | Ok p -> Fpath.(normalize p |> rem_ext |> basename)
    | Error _ -> failwith "Invalid ROM path"
  in
  Printf.printf "Opening ROM: '%s'\n" file_name;
  let rom, hash = read_file path in
  let config = read_header rom in
  Printf.printf "Mapper %d\n" config.mapper_nb;
  Printf.printf "PRG ROM is %d bytes\n" config.prg_rom_size;
  Printf.printf "CHR ROM is %d bytes\n" config.chr_rom_size;
  Printf.printf "PRG RAM : %B\n" config.prg_ram_present;
  if config.tv_system = `PAL then raise (Invalid_ROM "PAL is not supported");
  let cur_address = ref 0x10 in
  let trainer =
    if not config.trainer then None
    else (
      cur_address := !cur_address + 0x200;
      Some (Array.sub rom 0x10 512))
  in
  let prg_rom = Array.sub rom !cur_address config.prg_rom_size in
  cur_address := !cur_address + config.prg_rom_size;
  let chr_rom = Array.sub rom !cur_address config.chr_rom_size in
  cur_address := !cur_address + config.chr_rom_size;
  (* ignored PRG_RAM, Playchoices data, title *)
  { file_name; hash; config; prg_rom; chr_rom; trainer }
