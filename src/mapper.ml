open Infix_int.Common

module type S' = C6502.MemoryMap with type input := Rom.t

module type S = sig
  module CPU : S'
  module PPU : S'
end

module PPU_Basic = struct
  type mirroring = Horizontal | Vertical
  type t = { m : U8.t array; mirroring : mirroring }

  let create rom =
    let open Rom in
    let mirroring = if rom.config.mirroring then Vertical else Horizontal in
    let m = Array.make 0x4000 0u in
    Array.blit
      (Array.map C6502.Utils.u8 rom.chr_rom)
      0 m 0x0 rom.config.chr_rom_size;
    { m; mirroring }

  let nametable_mirroring t v =
    match t.mirroring with
    | Horizontal -> U16.(v $& ?~0x400U)
    | Vertical -> U16.(v $& ?~0x800U)
  (*| Single -> U16.(v $& ?~0xC00U)*)

  let mirroring t v =
    if v <= 0x1FFFU then v
    else if v <= 0x2FFFU then nametable_mirroring t v
    else v

  open U16

  let read t a = t.m.(?%(mirroring t a))
  let write t a v = t.m.(?%(mirroring t a)) <- v
end

module NROM : S = struct
  module CPU = struct
    type t = U8.t array

    let create rom =
      let open Rom in
      let bank_nb = rom.config.prg_rom_size / 0x4000 in
      if bank_nb = 2 then Array.map C6502.Utils.u8 rom.prg_rom
      else
        let m = Array.make 0x8000 0x00 in
        (* 32K *)
        Array.blit rom.prg_rom 0 m 0 0x4000;
        Array.blit rom.prg_rom 0 m 0x4000 0x4000;
        Array.map C6502.Utils.u8 m

    open U16

    let read t a = t.(?%(a $& 0x7FFFU))
    let write t a v = t.(?%(a $& 0x7FFFU)) <- v
  end

  module PPU = PPU_Basic
end

module UxROM : S = struct
  module CPU = struct
    type t = { banks : U8.t array array; mutable selected : int }

    let create rom =
      let open Rom in
      let bank_nb = rom.config.prg_rom_size / 0x4000 in
      let create_bank _ = Array.make 0x4000 0x00 in
      let banks = Array.init bank_nb create_bank in
      for i = 0 to bank_nb - 1 do
        Array.blit rom.prg_rom (0x4000 * i) banks.(i) 0 0x4000
      done;
      let banks = Array.map (Array.map C6502.Utils.u8) banks in
      { banks; selected = 0 }

    let last_bank t = t.banks.(Array.length t.banks - 1)

    open U16

    let read t a =
      if a >= 0xC000U then (last_bank t).(?%(a $& 0X3FFFU))
      else t.banks.(t.selected).(?%(a $& 0x3FFFU))

    let write t _ v = t.selected <- U8.to_int v
  end

  module PPU = PPU_Basic
end

let mappers = [ (0, (module NROM : S)); (2, (module UxROM)) ]

let find rom =
  let open Rom in
  match List.assoc_opt rom.config.mapper_nb mappers with
  | None -> raise (Invalid_ROM "Unsupported mapper")
  | Some x -> x
