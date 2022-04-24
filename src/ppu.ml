open Infix_int.Common

(* Important links
 * - Memory map : https://wiki.nesdev.com/w/index.php/PPU_memory_map
 * - Rendering : https://wiki.nesdev.com/w/index.php/PPU_rendering
 * - Scrolling : https://wiki.nesdev.com/w/index.php/PPU_scrolling
 * - Pattern tables : https://wiki.nesdev.com/w/index.php/PPU_pattern_tables
 * - Name tables : https://wiki.nesdev.com/w/index.php/PPU_nametables
 * - Attribute tables : https://wiki.nesdev.com/w/index.php/PPU_attribute_tables
 * - Shift registers topic : https://forums.nesdev.com/viewtopic.php?t=10348
 * - Some more details on rendering : https://fceux.com/web/help/PPU.html
 *)

let palette = 
    [0x7C7C7C; 0x0000FC; 0x0000BC; 0x4428BC; 0x940084; 0xA80020; 0xA81000; 0x881400;
     0x503000; 0x007800; 0x006800; 0x005800; 0x004058; 0x000000; 0x000000; 0x000000;
     0xBCBCBC; 0x0078F8; 0x0058F8; 0x6844FC; 0xD800CC; 0xE40058; 0xF83800; 0xE45C10;
     0xAC7C00; 0x00B800; 0x00A800; 0x00A844; 0x008888; 0x000000; 0x000000; 0x000000;
     0xF8F8F8; 0x3CBCFC; 0x6888FC; 0x9878F8; 0xF878F8; 0xF85898; 0xF87858; 0xFCA044;
     0xF8B800; 0xB8F818; 0x58D854; 0x58F898; 0x00E8D8; 0x787878; 0x000000; 0x000000;
     0xFCFCFC; 0xA4E4FC; 0xB8B8F8; 0xD8B8F8; 0xF8B8F8; 0xF8A4C0; 0xF0D0B0; 0xFCE0A8;
     0xF8D878; 0xD8F878; 0xB8F8B8; 0xB8F8D8; 0x00FCFC; 0xF8D8F8; 0x000000; 0x000000]

module State = struct

  module Mem = struct
    (* Main memory *)
    let main = Array.make 0x4000 0u
    let address = ref 0U
    let temp_address = ref 0U (* 15-bit *)
  end

  module OAM = struct
    let address = ref 0u

    let primary = Array.make 0x100 0u
    let secondary = Array.make 0x20 0u
    let next_open_slot = ref 0
    (* Contain pattern table data *)
    let render_shifters = Array.init 8 (fun _ -> (ref 0u, ref 0u))
    (* Contain attribute bytes *)
    let latches = Array.make 8 0u
    (* Contain X positions *)
    let counters = Array.make 8 0u

    (* Fetching state machine *)
    module SM = struct
      type t =
        | Sleep of t (* rest 1 cycle, remember next state *)
        | CopyY (* read a sprite's Y-coordinate (OAM[n][0], copying it to the
                   next open slot in secondary OAM *)
        | CopyRest of int (* copy remaining bytes OAM[n[1] thru OAM[n][3] *)
        | OverflowDetection of int (* buggy overflow detection loop *)
        | Full (* do not copy anything, increment n *)

      let state = ref CopyY
      let n = ref 0
    end
  end

  (* Control register *)
  module Control = struct
    let increment = ref 1U
    let sprite_pattern_address = ref 0x0000U
    let background_pattern_address = ref 0x0000U
    let sprite_size = ref false
    let master_slave_mode = ref false
    let nmi_enabled = ref false
  end

  (* Mask register *)
  module Graphics = struct
    let greyscale = ref false
    let background_leftmost = ref false
    let sprites_leftmost = ref false
    let background = ref false
    let sprites = ref false
    let emph_red = ref false
    let emph_green = ref false
    let emph_blue = ref false
  end

  (* Status register *)
  module Status = struct
    let sprite_0_hit = ref false
    let vblank_enabled = ref true
    let vbl_read = ref false
  end

  module Rendering = struct
    let frame = ref 0
    let scanline = ref 261
    let cycle = ref 0

    module BG = struct
      (* For storing pattern table data for two tiles *)
      let low, high = ref 0U, ref 0U
      let next_low, next_high = ref 0u, ref 0u
    end
    module AT = struct
      (* For storing palette attributes, not shifting *)
      let low, high = ref 0u, ref 0u
      let low_next, high_next = ref false, ref false
      let next = ref 0u
    end
    module NT = struct
      let next = ref 0u
    end
  end

  (* Set at init *)
  let mirroring_mode = ref false (* 0 : horizon (vert arr) *)
  let interrupt_cpu = ref None

  (* Scrolling *)
  let fine_x_scroll = ref 0u

  (* Latch for PPUSCROLL and PPUADDR *)
  let latch = ref true (* True : first set *)

  let bus_latch = ref 0x00u

  let vram_buffer = ref 0u
end

open State

let is_rendering () = !Graphics.background || !Graphics.sprites

let read_latch () =
  let r = !latch in
  latch := not !latch;
  r

let int_of_bool b = if b then 1 else 0
let nth_bit b n = U8.(b $& (1u $<< n) <> 0u)
let reverse_byte b =
  let cur = ref b in
  let res = ref 0u in
  let open U8 in
  for _ = 1 to 8 do
    res := !res $<< 1;
    res := !res + (!cur $& 1u);
    cur := !cur $>> 1;
  done;
  !res

(* PPU memory address with redirections *)
let address_indirection v =
  let open U16 in
  if v <= 0x2FFFU then v
  else if v <= 0x3EFFU then v $& 0x2FFFU
  else 
    let v = v $& 0x3F1FU in
    if v $& 0x7U = 0U then v $& ?~0x10U else v

let set_ppu v x = Mem.main.(address_indirection v |> U16.to_int) <- x
let get_ppu v = Mem.main.(address_indirection v |> U16.to_int)

let increment_ppu_address () = Mem.address := U16.(!Mem.address + !Control.increment)

open C6502.Int_utils
let set_register register (v : U8.t) =
  let open U16 in
  bus_latch := v ;
  match register with
  | 0 -> (* Control register *)
    (* t: ...GH.. ........ <- d: ......GH *)
    let to_set = ?$ U8.(v $& 3u) $<< 10 in
    let with_hole = !Mem.temp_address $& 0x73FFU in
    Mem.temp_address := to_set $| with_hole;
    Control.increment := if nth_bit v 2 then 32U else 1U;
    Control.sprite_pattern_address := if (nth_bit v 3) then 0x1000U else 0U;
    Control.background_pattern_address := if (nth_bit v 4) then 0x1000U else 0U;
    Control.sprite_size := nth_bit v 5;
    Control.master_slave_mode := nth_bit v 6;
    Control.nmi_enabled := nth_bit v 7
  | 1 -> (* Mask register *)
    Graphics.greyscale := nth_bit v 0;
    Graphics.background_leftmost := nth_bit v 1;
    Graphics.sprites_leftmost := nth_bit v 2;
    Graphics.background := nth_bit v 3;
    Graphics.sprites := nth_bit v 4;
    Graphics.emph_red := nth_bit v 5;
    Graphics.emph_green := nth_bit v 6;
    Graphics.emph_blue := nth_bit v 7
  | 3 -> (* OAM address *)
    OAM.address := v
  | 4 -> (* OAM data *)
    OAM.primary.(U8.to_int !OAM.address) <- v;
    OAM.address := U8.(succ !OAM.address)
  | 5 -> (* Scroll register *)
    if read_latch () then
      (* t: ....... ...ABCDE <- d: ABCDE... *)
      let to_set = ?$ v $>> 3 in
      let with_hole = !Mem.temp_address $& 0xFFFE0U in
      Mem.temp_address := to_set $| with_hole;
      (* x:              FGH <- d: .....FGH *)
      fine_x_scroll := U8.(v $& 7u)
    else
      (* t: FGH..AB CDE..... <- d: ABCDEFGH *)
      let fgh = ?$ U8.(v $& 7u) $<< 12 in
      let abcde = ?$ U8.(v $& 0xF8u) $<< 2 in
      let with_hole = !Mem.temp_address $& 0xC1FU in
      let with_fgh = with_hole $| fgh in
      Mem.temp_address := with_fgh $| abcde
  | 6 -> (* PPU address *)
    if read_latch () then
      (* t: .CDEFGH ........ <- d: ..CDEFGH
         <unused>     <- d: AB......
         t: Z...... ........ <- 0 (bit Z is cleared) *)
      Mem.temp_address := 
          (mk_addr ~lo:(get_lo !Mem.temp_address) ~hi:v) $& 0x3FFFU
    else (
      (* t: ....... ABCDEFGH <- d: ABCDEFGH
         v: <...all bits...> <- t: <...all bits...> *)
      Mem.temp_address := mk_addr ~hi:(get_hi !Mem.temp_address) ~lo:v ;
      Mem.address := !Mem.temp_address
    )
  | 7 -> (* PPU data *)
    set_ppu !Mem.address v;
    increment_ppu_address ()
  | _ -> Printf.printf "Warning: trying to set PPU register %d\n" register

let get_register reg =
  let open U16 in
  let res = match reg with
  | 2 -> (* Status register *)
    latch := true;
    let r =
      (int_of_bool !Status.vblank_enabled) lsl 7 lor
      (int_of_bool !Status.sprite_0_hit) lsl 6 in
    Status.vbl_read := true;
    Status.vblank_enabled := false; (u8 r)
  | 4 -> (* OAM data *)
    OAM.primary.(U8.to_int !OAM.address)
  | 7 -> (* PPU data *)
    (* Palette mirroring *)
    let addr = address_indirection !Mem.address in
    Mem.address := (!Mem.address + !Control.increment) $& 0x3FFFU;
    (* Correct buffer *)
    if addr >= 0x3F00U then begin
      (* TODO what is this ? why are we doing this ? *)
      vram_buffer := get_ppu (addr $& 0x2F1FU);
      get_ppu addr
    end else begin
      let old = !vram_buffer in
      vram_buffer := get_ppu addr;
      old
    end
  | _ -> !bus_latch
  in bus_latch := res ; res

let dma read cpu_begin =
  let rec aux cpu_addr oam_addr length =
    if length > 0 then (
      OAM.primary.(U8.to_int oam_addr) <- read cpu_addr;
      aux U16.(succ cpu_addr) U8.(succ oam_addr) (length - 1)
    )
  in aux cpu_begin !OAM.address 0x100

module Rendering = struct
  open Rendering (* open state *)

  let draw_pixel disp x y pal_start ~pal:palette_nb ~pat:color_nb =
    if color_nb <> 0u then
      (* Get a palette address, a palette number and a color number, give the
       * corresponding color *)
      let address =
        U16.(pal_start + (u16of8 palette_nb) * 4U + (u16of8 color_nb)) in
      let color = get_ppu address in
      Display.set_pixel disp ~x:(U8.to_int x) ~y:(U8.to_int y) ~color

  let copy_bits_16 ~src ~dst mask =
    let open U16 in
    let to_set = src $& mask in
    let with_hole = dst $& ?~mask in
    to_set $| with_hole

  let inc_hori v =
    let open U16 in
    let coarse = !v $& 0x001FU in
    if coarse = 0x1FU then (
      v := !v $& ?~0x1FU;
      v := !v $^ 0x0400U;
    )
    else v := !v + 1U

  let inc_vert v =
    let open U16 in
    if !v $& 0x7000U <> 0x7000U then
      v := !v + 0x1000U
    else (
      v := !v $& ?~0x7000U;
      let y = ref ((!v $& 0x03E0U) $>> 5) in
      if !y = 29U then (
        y := 0U;
        v := !v $^ 0x0800U
      )
      else if !y = 31U then (
        y := 0U
      )
      else (
        y := !y + 1U
      ) ;
      v := (!v $& ?~0x03E0U) $| (!y $<< 5);
    )

  let pat_address ~bank ~offset =
    let open U16 in
    let v = !Mem.address in
    let offset = offset * 16U in
    let finey = (v $& 0x7000U) $>> 12 in
    bank + offset + finey

  let fetch_next_data () =
    (* Step number in the fetching process *)
    let local_step = !cycle mod 8 in
    let open U16 in
    (* Every 8 cycles (9, 17, 25, ..., 257 *)
    (* Actual memory fetching *)
    (* Only 12 first bits of address should be used *)
    begin match local_step with
      | 0 ->
        inc_hori Mem.address;
        if !cycle = 256 then (inc_vert Mem.address)
      | 1 ->
        if !cycle <> 0 then (
          (* Reload shifters *)
          BG.low := mk_addr ~lo:!BG.next_low ~hi:(get_hi !BG.low) ;
          BG.high := mk_addr ~lo:!BG.next_high ~hi:(get_hi !BG.high) ;
          AT.low_next := U8.(0x1u $& !AT.next <> 0u);
          AT.high_next := U8.(0x2u $& !AT.next <> 0u)
        );
        (* load NT byte to shift8_nt_next *)
        let v = !Mem.address in
        let tile_address = 0x2000U $| (v $& 0xFFFU) in
        NT.next := get_ppu tile_address
      | 3 -> (* load AT byte to shift8_at_next *)
        (* stolen from mesen *)
        let v = !Mem.address in
        let addr = 0x23C0U $| (v $& 0x0C00U) $| ((v $>> 4) $& 0x38U)
                   $| ((v $>> 2) $& 0x7U) in
        let data = ?$ (get_ppu addr) in
        let shift = ?% ((0x04U $& (v $>> 4)) $| (v $& 0x02U)) in
        AT.next := U8.(?$$) (0x3U $& (data $>> shift))
      | 5 -> (* load low BG tile byte to next_bg_low (pattern table) *)
        let offset = ?$ !NT.next in
        let addr = pat_address ~bank:!Control.background_pattern_address ~offset
        in
        BG.next_low := get_ppu addr
      | 7 -> (* load high BG tile byte to next_bg_high *)
        let offset = ?$ !NT.next in
        let addr = pat_address ~bank:!Control.background_pattern_address ~offset
        in
        BG.next_high := get_ppu (addr + 8U)
      | _ -> ()
    end

  let decrease_sprite_counters () =
    for i = 0 to 7 do
      let v = OAM.counters.(i) in
      if v <> 0u then
        OAM.counters.(i) <- U8.(pred v)
    done

  let combine8 ~low ~high =
    let open U8 in
    ((high $<< 1) $| (low $& 1u)) $& 3u

  let sprite_pixel () =
    let found = ref false in
    let color = ref (0u, 0u, false) in
    if !Graphics.sprites then (
      for i = 0 to 7 do
        (* sprite is active *)
        if (not !found) && OAM.counters.(i) = 0u then (
          let low, high = OAM.render_shifters.(i) in
          let scroll = 7 in
          let pat = combine8 ~low:U8.(!low $>> scroll) ~high:U8.(!high $>> scroll) in
          (* opaque pixel *)
          if pat <> 0u then (
            found := true;
            let pal = U8.(OAM.latches.(i) $& 0x3u) in
            let priority = nth_bit OAM.latches.(i) 5 in
            color := (pat, pal, priority)
          )
        )
      done
    ); !color

  let render_pixel disp =
    let x = U8.of_int (!cycle - 1) in
    let y = U8.of_int !scanline in
    let pat, pal = if !Graphics.background then
      let scroll1 = 15 - U8.to_int !fine_x_scroll in
      let scroll2 = 7 - U8.to_int !fine_x_scroll in
      let open U16 in
      let patl = (!BG.low $>> scroll1) $& 0x1U in
      let path = (!BG.high $>> scroll1) $& 0x1U in
      let pat = U8.(?$$) (patl $| (path $<< 1)) in
      let open U8 in
      let pal = combine8 ~low:(!AT.low $>> scroll2) ~high:(!AT.high $>> scroll2)
      in
      (pat, pal)
      else (0u, 0u)
    in
    let spat, spal, priority = sprite_pixel () in
    match U8.(?% pat, ?% spat, priority) with
    | _, 0, _ -> draw_pixel disp x y 0x3F00U ~pal ~pat
    | 0, _, _ -> draw_pixel disp x y 0x3F10U ~pal:spal ~pat:spat
    | _, _, false ->
      Status.sprite_0_hit := true;
      draw_pixel disp x y 0x3F10U ~pal:spal ~pat:spat
    | _, _, true ->
      Status.sprite_0_hit := true;
      draw_pixel disp x y 0x3F00U ~pal ~pat

  let shift_registers () =
    BG.low := U16.(!BG.low $<< 1);
    BG.high := U16.(!BG.high $<< 1);
    AT.low := U8.(!AT.low $<< 1);
    AT.high := U8.(!AT.high $<< 1);
    AT.low := if !AT.low_next then U8.(!AT.low $| 1u) else !AT.low;
    AT.high := if !AT.high_next then U8.(!AT.high $| 1u) else !AT.high;
    for i = 0 to 7 do
      (* sprite is active *)
      if OAM.counters.(i) = 0u then (
        let low, high = OAM.render_shifters.(i) in
        low := U8.(!low $<< 1);
        high := U8.(!high $<< 1);
      )
    done

  let data_fetching disp render =
    (* Cycle 0 : IDLE *)
    if !cycle = 0 then ()
    (* Cycles 1 - 256 : BACKGROUND FETCHING *)
    else if !cycle <= 256 && is_rendering () then (
      fetch_next_data ();
      (* Pixel rendering *)
      if render && (!cycle > 8 || !Graphics.background_leftmost) then (
        render_pixel disp
      );
      shift_registers ();
      decrease_sprite_counters ()
    )
    (* Cycles 257 - 320 : NEXT SPRITES FETCHING in another function *)
    else if !cycle <= 320 then (
      (* If rendering is enabled, the PPU copies all bits related to
       * horizontal position from t to v *)
      (* v: ....A.. ...BCDEF <- t: ....A.. ...BCDEF *)
      if !cycle = 257 && is_rendering () then (
        Mem.address :=
          copy_bits_16 ~src:!Mem.temp_address ~dst:!Mem.address 0x41FU
      )
    )
    (* Cycles 321 - 336 : NEXT TWO TILES FETCHING *)
    else if !cycle <= 336 && is_rendering () then (
      fetch_next_data () ;
      shift_registers ()
    )
    (* Cycles 337-340 : USELESS *)
    else ()

  let get_oam_byte n m = OAM.primary.(4 * n + m)

  let y_in_range y_pos =
    let offset = if !Control.sprite_size then 16 else 8 in
    let y_pos = U8.to_int y_pos in
    y_pos <= !scanline && !scanline < y_pos + offset

  let sprite_evaluation () =
    let open OAM in
    let set_next s = SM.state := Sleep s in
    let decide_next () =
      if !SM.n = 0 then SM.Full
      else if !next_open_slot < 32 then SM.CopyY
      else SM.OverflowDetection 0
    in
    match !SM.state with
    | Sleep next -> SM.state := next
    | CopyY ->
      let y_pos = get_oam_byte !SM.n 0 in
      secondary.(!next_open_slot) <- y_pos;
      if y_in_range y_pos then (
        incr next_open_slot;
        set_next (CopyRest 1)
      ) else (
        SM.n := (!SM.n + 1) land 0x3f;
        set_next (decide_next ())
      )
    | CopyRest m ->
      secondary.(!next_open_slot) <- get_oam_byte !SM.n m;
      incr next_open_slot;
      if m = 3 then (
        SM.n := (!SM.n + 1) land 0x3f;
        set_next (decide_next ())
      ) else set_next (CopyRest (m + 1))
    | OverflowDetection _ -> () (* TODO *)
    | Full -> ()

  let fetch_sprite () =
    let get_oam'_byte n m = OAM.secondary.(4 * n + m) in
    let sn = (!cycle - 257) / 8 in
    let step = (!cycle - 257) mod 8 in
    let enabled = sn < (!OAM.next_open_slot / 4) in
    let fetch_tile_8 ~high =
      let y_pos = get_oam'_byte sn 0 in
      let fine_offset = U16.(?@ !scanline - ?$ y_pos) in
      (* Flip vertically *)
      let fine_offset = if nth_bit OAM.latches.(sn) 7 then
          U16.(7U - fine_offset)
        else fine_offset
      in
      let index = get_oam'_byte sn 1 in
      let bank = !Control.sprite_pattern_address in
      let open U16 in
      let offset = of_uint8 index in
      let offset = offset * 16U in
      let addr = bank + offset + fine_offset in
      let addr = if high then U16.(addr + 8U) else addr in
      let data = get_ppu addr in
      (* Flip horizontally *)
      if nth_bit OAM.latches.(sn) 6 then reverse_byte data else data
    in
    let fetch_tile_16 ~high =
      let y_pos = get_oam'_byte sn 0 in
      let row = !scanline - U8.(?% y_pos) in
      let vert_flip = nth_bit OAM.latches.(sn) 7 in
      let tile_nb = if row < 8 <> vert_flip then 0U else 1U in
      let y_offset = U16.(?@ (row mod 8)) in
      let y_offset = if vert_flip then U16.(7U - y_offset) else y_offset in
      let index = get_oam'_byte sn 1 in
      let bank = if nth_bit index 0 then 0x1000U else 0x0U in
      let index' = U16.((U8.(index $& ?~1u) |> U16.of_uint8) + tile_nb) in
      let offset = U16.(index' * 16U) in
      let open U16 in
      let addr = bank + offset + y_offset in
      let addr = if high then U16.(addr + 8U) else addr in
      let data = get_ppu addr in
      (* Flip horizontally *)
      if nth_bit OAM.latches.(sn) 6 then reverse_byte data else data
    in
    let fetch = if !Control.sprite_size then fetch_tile_16 else fetch_tile_8 in
    match step with
    | 2 -> OAM.latches.(sn) <- get_oam'_byte sn 2
    | 3 -> OAM.counters.(sn) <- get_oam'_byte sn 3
    | 4 when enabled -> (fst OAM.render_shifters.(sn)) := fetch ~high:false
    | 6 when enabled -> (snd OAM.render_shifters.(sn)) := fetch ~high:true
    | 4 -> (fst OAM.render_shifters.(sn)) := 0u
    | 6 -> (fst OAM.render_shifters.(sn)) := 0u
    | _ -> ()

  let sprite_fetching () =
    (* Cycle 0: IDLE *)
    if !cycle = 0 then ()
    (* Cycles 1-64: clear OAM' *)
    else if !cycle <= 64 && !cycle mod 2 = 0 then
      OAM.secondary.((!cycle - 1) / 2) <- 0xFFu
    (* Cycles 65-256: sprite evaluation *)
    else if !cycle <= 256 then (
      if !cycle = 65 then (
        OAM.SM.state := CopyY;
        OAM.next_open_slot := 0;
        OAM.SM.n := 0
      );
      sprite_evaluation ();
    )
    (* Cycles 257-320: sprite tile fetching *)
    else if !cycle <= 320 then (
      fetch_sprite ()
    )
    (* Cycles 321-340: IDLE *)
    else ()

  let next_cycle disp =
    (* Process *)
    (* Visible scanlines : 0 - 239 *)
    if !scanline <= 239 then (
      sprite_fetching ();
      data_fetching disp true;
    )
    (* Post-render scanline : 240  (IDLE) *)
    else if !scanline = 240 then ()
    (* Vertical blanking *)
    else if !scanline = 241 && !cycle = 1 then (
      if not !Status.vbl_read then Status.vblank_enabled := true;
      if !Control.nmi_enabled && not !Status.vbl_read then
        Option.get !interrupt_cpu ()
    )
    (* Last vertical blanking lines : IDLE *)
    else if !scanline <= 260 then ()
    (* Pre-rendering scanline *)
    else (
      (* Clear VBLANK *)
      if !cycle = 1 then (
        Status.vblank_enabled := false
      );
      (* Fetch data for next frame *)
      sprite_fetching ();
      data_fetching disp false;
      (*  If rendering is enabled, at the end of vblank, shortly after
       *  the horizontal bits are copied from t to v at dot 257, the PPU
       *  will repeatedly copy the vertical bits from t to v from dots
       *  280 to 304, completing the full initialization of v from t: *)
      if !cycle >= 280 && !cycle <= 304 && is_rendering () then (
        Mem.address :=
          copy_bits_16 ~src:!Mem.temp_address ~dst:!Mem.address 0x7BE0U
      )
      (* Final dot : change everything *)
      else if !cycle = 340 then (
        Status.sprite_0_hit := false ; 
        incr frame ;
        (*render_sprites true 0 ;*)
        Display.render disp;
        Display.clear disp Mem.main.(0x3F00);
        (*render_sprites false 0 ;*)
        (* this should be at cycle 1 *)
        (* Odd frame : jump to (0, 0) directly *)
        if (!frame mod 2) = 1 && is_rendering () then (
          scanline := 0;
          cycle := 0
        )
      )
    ) ;
    (* Next cycle *)
    incr cycle;
    if !cycle = 341 then (
      cycle := 0;
      (* Next scanline *)
      incr scanline ;
      if !scanline = 262 then (
        (* End of frame *)
        scanline := 0;
      )
    ) ;
    Status.vbl_read := false
end

let init ic mm =
  interrupt_cpu := Some ic;
  mirroring_mode := mm;
  Display.create ~width:256 ~height:240 ~scale:4 ~palette "NES"

let next_cycle = Rendering.next_cycle

let exit t =
  Display.delete t;
  Display.exit ()

module Debug = struct
  type t = {
    names : Display.t;
    attributes : Display.t;
    patterns : Display.t;
  }

  let pal_4 = [0x000000; 0xFF0000; 0x00FF00; 0x0000FF]

  let init () = {
    names = Display.create ~width:64 ~height:62 ~scale:8 ~palette "Name tables + palettes";
    attributes = Display.create ~width:32 ~height:32
        ~scale:16 ~palette:pal_4 "Attribute tables";
    patterns = Display.create ~width:256 ~height:128
        ~scale:4 ~palette:pal_4 "Pattern tables"
  }

  let delete t =
    Display.delete t.names;
    Display.delete t.attributes;
    Display.delete t.patterns

  let cooldown = 1000
  let counter = ref 0

  let render_nametables disp =
    let set_nametable addr x_orig y_orig =
      for y = 0 to 29 do
        for x = 0 to 31 do
          let addr = addr + y * 32 + x in
          let v = Mem.main.(addr) in
          Display.set_pixel disp ~x:(x + x_orig) ~y:(y + y_orig) ~color:v
        done
      done
    in
    set_nametable 0x2000 0 0;
    set_nametable 0x2400 32 0;
    set_nametable 0x2800 0 30;
    set_nametable 0x2C00 32 30;
    for x = 0 to 63 do
      Display.set_pixel disp ~x ~y:60 ~color:(U8.of_int x)
    done;
    for x = 0 to 31 do
      let color = Mem.main.(0x3F00 + x) in
      Display.set_pixel disp ~x:(x * 2) ~y:61 ~color;
      Display.set_pixel disp ~x:(x * 2 + 1) ~y:61 ~color
    done

  let render_attributes disp =
    let set_attr addr x_orig y_orig =
      for y = 0 to 7 do
        for x = 0 to 7 do
          let addr = addr + y * 8 + x in
          let v = Mem.main.(addr) in
          let x' = x * 2 + x_orig in
          let y' = y * 2 + y_orig in
          let open U8 in
          let top_left = v $& 0x3u in
          let top_right = (v $>> 2) $& 0x3u in
          let bot_left = (v $>> 4) $& 0x3u in
          let bot_right = (v $>> 6) $& 0x3u in
          let open Stdlib in
          Display.set_pixel disp ~x:x' ~y:y' ~color:top_left;
          Display.set_pixel disp ~x:(x'+1) ~y:y' ~color:top_right;
          Display.set_pixel disp ~x:x' ~y:(y'+1) ~color:bot_left;
          Display.set_pixel disp ~x:(x'+1) ~y:(y'+1) ~color:bot_right;
        done
      done
    in
    set_attr 0x23C0 0 0;
    set_attr 0x27C0 16 0;
    set_attr 0x2BC0 0 16;
    set_attr 0x2FC0 16 16

  let render_patterns disp =
    let set_pattern addr x_orig y_orig =
      for tile_y = 0 to 15 do
        for tile_x = 0 to 15 do
          let addr = addr + (tile_y * 16 + tile_x) * 16 in
          for y = 0 to 7 do
            for x = 0 to 7 do
              let low = Mem.main.(addr + y) in
              let high = Mem.main.(addr + y + 8) in
              let shift = 7 - x in
              let open U8 in
              let low = (low $>> shift) $& 1u in
              let high = (high $>> shift) $& 1u in
              let color = low $| (high * 2u) in
              let open Stdlib in
              Display.set_pixel disp
                ~x:(tile_x * 8 + x + x_orig) ~y:(tile_y * 8 + y + y_orig) ~color
            done
          done
        done
      done
    in
    set_pattern 0x0000 0 0;
    set_pattern 0x1000 128 0

  let render = function
    | None -> ()
    | Some {names; attributes; patterns} ->
      if !counter = 0 then (
        render_nametables names;
        render_attributes attributes;
        render_patterns patterns;
        Display.render names;
        Display.render attributes;
        Display.render patterns;
        counter := cooldown
      );
      decr counter;
end
