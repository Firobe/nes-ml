open Infix_int.Common

module type S' = sig
  type t

  val read : t -> U16.t -> U8.t
  val write : t -> U16.t -> U8.t -> unit
end

module type S = sig
  type t

  val create : Rom.t -> t

  module CPU : S' with type t := t
  module PPU : S' with type t := t
end

module PPU_Basic = struct
  type mirroring = Horizontal | Vertical
  type t = { m : U8.t array; mirroring : mirroring }

  module Make (A : sig
    type outer

    val get : outer -> t
  end) =
  struct
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

    let mirroring t v =
      if v <= 0x1FFFU then v
      else if v <= 0x2FFFU then nametable_mirroring t v
      else v

    open U16

    let read t a = (A.get t).m.(?%(mirroring (A.get t) a))
    let write t a v = (A.get t).m.(?%(mirroring (A.get t) a)) <- v
  end
end

module NROM : S = struct
  type t = { prg : U8.t array; chr : PPU_Basic.t }

  module CPU = struct
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

    let read t a = t.prg.(?%(a $& 0x7FFFU))
    let write t a v = t.prg.(?%(a $& 0x7FFFU)) <- v
  end

  module PPU = PPU_Basic.Make (struct
    type outer = t

    let get { chr; _ } = chr
  end)

  let create rom = { prg = CPU.create rom; chr = PPU.create rom }
end

let banks_of_raw data total_size bank_size =
  let bank_nb = total_size / bank_size in
  let create_bank _ = Array.make bank_size 0x00 in
  let banks = Array.init bank_nb create_bank in
  for i = 0 to bank_nb - 1 do
    Array.blit data (bank_size * i) banks.(i) 0 bank_size
  done;
  Array.map (Array.map C6502.Utils.u8) banks

module UxROM : S = struct
  type prg = { banks : U8.t array array; mutable selected : int }
  type t = { prg : prg; chr : PPU_Basic.t }

  module CPU = struct
    let create rom =
      let open Rom in
      let banks = banks_of_raw rom.prg_rom rom.config.prg_rom_size 0x4000 in
      { banks; selected = 0 }

    let last_bank t = t.prg.banks.(Array.length t.prg.banks - 1)

    open U16

    let read t a =
      if a >= 0xC000U then (last_bank t).(?%(a $& 0x3FFFU))
      else t.prg.banks.(t.prg.selected).(?%(a $& 0x3FFFU))

    let write t _ v = t.prg.selected <- U8.to_int v
  end

  module PPU = PPU_Basic.Make (struct
    type outer = t

    let get { chr; _ } = chr
  end)

  let create rom = { prg = CPU.create rom; chr = PPU.create rom }
end

module MMC1 : S = struct
  type prg = {
    banks : U8.t array array;
    mutable mode : int;
    mutable selected : int;
    prg_ram : U8.t array;
  }

  type chr = {
    banks : U8.t array array;
    mutable mode_4k : bool;
    mutable selected_0 : int;
    mutable selected_1 : int;
    ram : U8.t array;
  }

  type mirroring = Horizontal | Vertical | Single_lower | Single_upper

  type t = {
    prg : prg;
    chr : chr;
    mutable mirroring : mirroring;
    mutable shift_register : U8.t;
    mutable sr_writes : int;
  }

  let create rom =
    Rom.
      {
        prg =
          {
            banks = banks_of_raw rom.prg_rom rom.config.prg_rom_size 0x4000;
            mode = 3;
            selected = 0;
            prg_ram = Array.make 0x2000 0u;
          };
        chr =
          {
            banks =
              (if rom.config.chr_rom_size = 0 then
               (* CHR RAM *)
               [| Array.make 0x1000 0u; Array.make 0x1000 0u |]
              else banks_of_raw rom.chr_rom rom.config.chr_rom_size 0x1000);
            mode_4k = false;
            selected_0 = 0;
            selected_1 = 0;
            ram = Array.make 0x4000 0u;
          };
        mirroring = Single_lower;
        shift_register = 0u;
        sr_writes = 0;
      }

  module CPU = struct
    let write_control t v =
      let mirroring =
        match U8.(v $& 0x3u |> to_int) with
        | 0 -> Single_lower
        | 1 -> Single_upper
        | 2 -> Vertical
        | 3 -> Horizontal
        | _ -> assert false
      in
      (* Copy active table when switching mirroring
       * there's probably a better solution ...*)
      (match (t.mirroring, mirroring) with
      | Vertical, Horizontal ->
          Array.blit t.chr.ram 0x2400 t.chr.ram 0x2800 0x400
      | Horizontal, Vertical ->
          Array.blit t.chr.ram 0x2800 t.chr.ram 0x2400 0x400
      | _ -> ());
      t.mirroring <- mirroring;
      t.chr.mode_4k <- U8.(v $& 0x10u <> 0u);
      t.prg.mode <- U8.(v $& 0xCu $>> 2 |> to_int)

    let write_register t a v =
      let open U8 in
      match U16.(a $& 0xE000U |> to_int) with
      | 0x8000 -> write_control t v
      | 0xA000 ->
          if t.chr.mode_4k then
            t.chr.selected_0 <- ?%v mod Array.length t.chr.banks
          else t.chr.selected_0 <- ?%(v $& 0x1Eu) mod Array.length t.chr.banks
      | 0xC000 ->
          if t.chr.mode_4k then
            t.chr.selected_1 <- ?%v mod Array.length t.chr.banks
      | 0xE000 ->
          if t.prg.mode < 2 (* 32 Kb mode *) then
            t.prg.selected <- ?%(v $& 0xEu) mod Array.length t.prg.banks
          else t.prg.selected <- ?%v mod Array.length t.prg.banks
      | _ -> assert false

    let write t a v =
      if a < 0x8000U then U16.(t.prg.prg_ram.(?%a) <- v)
      else
        let b7 = U8.(v $>> 7) in
        if b7 = 1u then (
          t.shift_register <- 0u;
          t.sr_writes <- 0;
          t.prg.mode <- 3)
        else (
          t.shift_register <- U8.(t.shift_register $| (v $<< 7));
          t.shift_register <- U8.(t.shift_register $>> 1);
          t.sr_writes <- t.sr_writes + 1;
          if t.sr_writes = 5 then (
            t.sr_writes <- 0;
            let v = U8.(t.shift_register $>> 2) in
            t.shift_register <- 0u;
            write_register t a v))

    let read t a =
      let a = U16.(?%a) in
      if a < 0x8000 then t.prg.prg_ram.(a)
      else if a < 0xC000 then
        let selected = match t.prg.mode with 2 -> 0 | _ -> t.prg.selected in
        t.prg.banks.(selected).(a land 0x3FFF)
      else
        let selected =
          match t.prg.mode with
          | 0 | 1 -> (t.prg.selected + 1) mod Array.length t.prg.banks
          | 3 -> Array.length t.prg.banks - 1
          | 2 -> t.prg.selected
          | _ -> assert false
        in
        t.prg.banks.(selected).(a land 0x3FFF)
  end

  module PPU = struct
    let nametable_mirroring t v =
      match t.mirroring with
      | Horizontal -> U16.(v $& ?~0x400U)
      | Vertical -> U16.(v $& ?~0x800U)
      | Single_lower | Single_upper -> U16.(v $& ?~0xC00U)

    let indirection t v =
      if v <= 0x1FFFU then v
      else if v <= 0x2FFFU then nametable_mirroring t v
      else v

    let read t a =
      let a = indirection t a in
      let open U16 in
      if a <= 0x0FFFU then
        t.chr.banks.(t.chr.selected_0).(a $& 0x0FFFU |> to_int)
      else if a <= 0x1FFFU then
        if t.chr.mode_4k then
          t.chr.banks.(t.chr.selected_1).(a $& 0x0FFFU |> to_int)
        else
          t.chr.banks.(Stdlib.succ t.chr.selected_0 mod Array.length t.chr.banks).(
          a $& 0x0FFFU |> to_int)
      else t.chr.ram.(?%a)

    let write t a v =
      let a = indirection t a in
      let open U16 in
      if a <= 0x0FFFU then
        t.chr.banks.(t.chr.selected_0).(a $& 0x0FFFU |> to_int) <- v
      else if a <= 0x1FFFU then
        if t.chr.mode_4k then
          t.chr.banks.(t.chr.selected_1).(a $& 0x0FFFU |> to_int) <- v
        else
          t.chr.banks.(Stdlib.succ t.chr.selected_0 mod Array.length t.chr.banks).(
          a $& 0x0FFFU |> to_int) <- v
      else t.chr.ram.(?%a) <- v
  end
end

module MMC3 = struct
  type t = {
    (* (0: $8000-$9FFF swappable,
           $C000-$DFFF fixed to second-last bank;
        1: $C000-$DFFF swappable,
           $8000-$9FFF fixed to second-last bank)
    *)
    mutable prg_rom_mode : bool;
    (* (0: two 2 KB banks at $0000-$0FFF,
           four 1 KB banks at $1000-$1FFF;
        1: two 2 KB banks at $1000-$1FFF,
           four 1 KB banks at $0000-$0FFF)
    *)
    mutable chr_a12_inversion : bool;
    mutable next_register : int;
  }

  module CPU = struct end
  module PPU = struct end
end
[@@warning "-34"]

let mappers =
  [ (0, (module NROM : S)); (1, (module MMC1)); (2, (module UxROM)) ]

let find rom =
  let open Rom in
  match List.assoc_opt rom.config.mapper_nb mappers with
  | None -> raise (Invalid_ROM "Unsupported mapper")
  | Some x -> x
