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

type mirroring_kind =
  | Horizontal (* vertical arrangement *)
  | Vertical (* horizontal arrangement *)
  | Single
  | Quad

(** Main memory *)
module Mem = struct
  type t = {
    main : U8.t array;
    mutable nt_mirroring : mirroring_kind;
    mutable address : U16.t;
    mutable temp_address : U16.t;
    mutable latch : bool;
    mutable bus_latch : U8.t;
  }

  let create nt_mirroring = {
    main = Array.make 0x4000 0u;
    nt_mirroring;
    address = 0U;
    temp_address = 0U; (* 15-bit *)
    latch = true; (* True : first set *)
    bus_latch = 0x00u;
  }

  let read_latch t =
    let r = t.latch in
    t.latch <- not t.latch;
    r

  let nametable_mirroring t v =
    match t.nt_mirroring with
    | Horizontal -> U16.(v $& ?~0x400U)
    | Vertical -> U16.(v $& ?~0x800U)
    | Single -> U16.(v $& ?~0xC00U)
    | Quad -> failwith "4-screen mirroring not implemented"

  (* PPU memory address with redirections *)
  let address_indirection t v =
    let open U16 in
    if v <= 0x1FFFU then v
    else if v <= 0x2FFFU then nametable_mirroring t v
    else if v <= 0x3EFFU then v $& 0x2FFFU
    else 
      let v = v $& 0x3F1FU in
      if v $& 0x7U = 0U then v $& ?~0x10U else v

  let set t x = t.main.(address_indirection t t.address |> U16.to_int) <- x
  let get t v = t.main.(address_indirection t v |> U16.to_int)
end

module OAM = struct
  (* Fetching state machine *)
  module SM = struct
    type state =
      | Sleep of state (* rest 1 cycle, remember next state *)
      | CopyY (* read a sprite's Y-coordinate (OAM[n][0], copying it to the
                 next open slot in secondary OAM *)
      | CopyRest of int (* copy remaining bytes OAM[n[1] thru OAM[n][3] *)
      | OverflowDetection of int (* buggy overflow detection loop *)
      | Full (* do not copy anything, increment n *)

    type t = {
      mutable state : state;
      mutable n : int;
    }

    let create () = {
      state = CopyY;
      n = 0;
    }
  end

  type t = {
    primary : U8.t array;
    secondary : U8.t array;
    mutable address : U8.t;
    sm : SM.t;
    mutable next_open_slot : int;
    (* Contain pattern table data *)
    render_shifters : (U8.t ref * U8.t ref) array;
    (* Contain attribute bytes *)
    latches : U8.t array;
    (* Contain X positions *)
    counters : U8.t array;
    (* For optimisation *)
    (* True if on the current scanline, shifter 0 contains sprite 0 data *)
    mutable sprite_0_here : bool;
    (* Number of spries to display this scanline, for optimization purposes *)
    mutable last_sprite_this_scanline : int
  }

  let create () = {
    address = 0u;
    primary = Array.make 0x100 0u;
    secondary = Array.make 0x20 0u;
    next_open_slot = 0;
    sm = SM.create ();
    render_shifters = Array.init 8 (fun _ -> (ref 0u, ref 0u));
    latches = Array.make 8 0u;
    counters = Array.make 8 0u;
    sprite_0_here = false;
    last_sprite_this_scanline = 0;
  }

  let get_byte t n m = t.primary.(4 * n + m)

  let decrease_sprite_counters t =
    (* Opti *)
    for i = 0 to t.last_sprite_this_scanline do
      let v = t.counters.(i) in
      if v <> 0u then
        t.counters.(i) <- U8.(pred v)
    done

  let pixel t =
    let found = ref false in
    let color = ref (0u, 0u, false, false) in
    for i = 0 to t.last_sprite_this_scanline do
      (* sprite is active *)
      if (not !found) && t.counters.(i) = 0u then (
        let low, high = t.render_shifters.(i) in
        let scroll = 7 in
        let pat =
          U8.((((!high $>> scroll) $<< 1) $| ((!low $>> scroll) $& 1u)) $& 3u) in
        (* opaque pixel *)
        if pat <> 0u then (
          found := true;
          let pal = U8.(t.latches.(i) $& 0x3u) in
          let priority = nth_bit t.latches.(i) 5 in
          color := (pat, pal, priority, i = 0 && t.sprite_0_here)
        )
      )
    done;
    !color

end

(* Control register *)
module Control = struct
  type t = {
    mutable increment : U16.t;
    mutable sprite_pattern_address : U16.t;
    mutable background_pattern_address : U16.t;
    mutable sprite_size : bool;
    mutable master_slave_mode : bool;
    mutable nmi_enabled : bool
  }

  let create () = {
    increment = 1U;
    sprite_pattern_address = 0x0000U;
    background_pattern_address = 0x0000U;
    sprite_size = false;
    master_slave_mode = false;
    nmi_enabled = false;
  }
end

(* Mask register *)
module Graphics = struct
  type t = {
    mutable greyscale : bool;
    mutable background_leftmost : bool;
    mutable sprites_leftmost : bool;
    mutable background : bool;
    mutable sprites : bool;
    mutable emph_red : bool;
    mutable emph_green : bool;
    mutable emph_blue : bool;
  }

  let create () = {
    greyscale = false;
    background_leftmost = false;
    sprites_leftmost = false;
    background = false;
    sprites = false;
    emph_red = false;
    emph_green = false;
    emph_blue = false
  }

  let is_rendering t = t.background || t.sprites
end

(* Status register *)
module Status = struct
  type t = {
    mutable sprite_0_hit : bool;
    mutable vblank_enabled : bool;
    mutable vbl_read : bool;
  }

  let create () = {
    sprite_0_hit = false;
    vblank_enabled = true;
    vbl_read = false;
  }
end

module Rendering = struct
  module BG = struct
    (* For storing pattern table data for two tiles *)
    type t = {
      mutable low : U16.t;
      mutable high : U16.t;
      mutable next_low : U8.t;
      mutable next_high : U8.t;
    }
    let create () = {low = 0U; high = 0U; next_low = 0u; next_high = 0u}
  end
  module AT = struct
    type t = {
      (* For storing palette attributes *)
      mutable low : U8.t;
      mutable high : U8.t;
      mutable low_next : bool;
      mutable high_next : bool;
      mutable next : U8.t
    }
    let create () = {low = 0u; high = 0u;
                     low_next = false; high_next = false; next = 0u}
  end
  module NT = struct
    type t = {mutable next : U8.t}
    let create () = {next = 0u}
  end
  type t = {
    mutable frame : int;
    mutable scanline : int;
    mutable cycle : int;
    bg : BG.t;
    at : AT.t;
    nt : NT.t
  }

  let create () = {
    frame = 0;
    scanline = 261;
    cycle = 0;
    bg = BG.create ();
    at = AT.create ();
    nt = NT.create ()
  }
end

type t = {
  memory : Mem.t;
  oam : OAM.t;
  control : Control.t;
  graphics : Graphics.t;
  status : Status.t;
  rendering : Rendering.t;
  nmi : C6502.NMI.t;
  mutable fine_x_scroll : U8.t;
  mutable vram_buffer : U8.t;
  mutable should_render : [`No | `Yes of Stdint.uint8] 
}

let create mirroring_mode nmi = {
  memory = Mem.create mirroring_mode;
  oam = OAM.create ();
  control = Control.create ();
  graphics = Graphics.create ();
  status = Status.create ();
  rendering = Rendering.create ();
  nmi;
  (* Scrolling *)
  fine_x_scroll = 0u;
  (* Latch for PPUSCROLL and PPUADDR *)
  vram_buffer = 0u;
  should_render = `No
}

let init_memory t src size =
  Array.blit src 0 t.memory.main 0x0 size

let frame t = t.rendering.frame

let increment_ppu_address t =
  t.memory.address <- U16.((t.memory.address + t.control.increment) $& 0x3FFFU)

open C6502.Int_utils
let set_register t register (v : U8.t) =
  let open U16 in
  t.memory.bus_latch <- v ;
  match register with
  | 0 -> (* Control register *)
    (* t: ...GH.. ........ <- d: ......GH *)
    let to_set = ?$ U8.(v $& 3u) $<< 10 in
    let with_hole = t.memory.temp_address $& 0x73FFU in
    t.memory.temp_address <- to_set $| with_hole;
    t.control.increment <- if nth_bit v 2 then 32U else 1U;
    t.control.sprite_pattern_address <- if (nth_bit v 3) then 0x1000U else 0U;
    t.control.background_pattern_address <- if (nth_bit v 4) then 0x1000U else 0U;
    t.control.sprite_size <- nth_bit v 5;
    t.control.master_slave_mode <- nth_bit v 6;
    let old_nmi = t.control.nmi_enabled in
    t.control.nmi_enabled <- nth_bit v 7;
    (* If NMI enabled during VBLANK, interrupt now *)
    if t.control.nmi_enabled && not old_nmi
       && t.status.vblank_enabled && not t.status.vbl_read then
      C6502.NMI.pull t.nmi
  | 1 -> (* Mask register *)
    t.graphics.greyscale <- nth_bit v 0;
    t.graphics.background_leftmost <- nth_bit v 1;
    t.graphics.sprites_leftmost <- nth_bit v 2;
    t.graphics.background <- nth_bit v 3;
    t.graphics.sprites <- nth_bit v 4;
    t.graphics.emph_red <- nth_bit v 5;
    t.graphics.emph_green <- nth_bit v 6;
    t.graphics.emph_blue <- nth_bit v 7
  | 3 -> (* OAM address *)
    t.oam.address <- v
  | 4 -> (* OAM data *)
    t.oam.primary.(U8.to_int t.oam.address) <- v;
    t.oam.address <- U8.(succ t.oam.address)
  | 5 -> (* Scroll register *)
    if Mem.read_latch t.memory then
      (* t: ....... ...ABCDE <- d: ABCDE... *)
      let to_set = ?$ v $>> 3 in
      let with_hole = t.memory.temp_address $& 0xFFFE0U in
      t.memory.temp_address <- to_set $| with_hole;
      (* x:              FGH <- d: .....FGH *)
      t.fine_x_scroll <- U8.(v $& 7u)
    else
      (* t: FGH..AB CDE..... <- d: ABCDEFGH *)
      let fgh = ?$ U8.(v $& 7u) $<< 12 in
      let abcde = ?$ U8.(v $& 0xF8u) $<< 2 in
      let with_hole = t.memory.temp_address $& 0xC1FU in
      let with_fgh = with_hole $| fgh in
      t.memory.temp_address <- with_fgh $| abcde
  | 6 -> (* PPU address *)
    if Mem.read_latch t.memory then
      (* t: .CDEFGH ........ <- d: ..CDEFGH
         <unused>     <- d: AB......
         t: Z...... ........ <- 0 (bit Z is cleared) *)
      t.memory.temp_address <- 
          (mk_addr ~lo:(get_lo t.memory.temp_address) ~hi:v) $& 0x3FFFU
    else (
      (* t: ....... ABCDEFGH <- d: ABCDEFGH
         v: <...all bits...> <- t: <...all bits...> *)
      t.memory.temp_address <- mk_addr ~hi:(get_hi t.memory.temp_address) ~lo:v ;
      t.memory.address <- t.memory.temp_address
    )
  | 7 -> (* PPU data *)
    Mem.set t.memory v;
    increment_ppu_address t
  | _ -> Printf.printf "Warning: trying to set PPU register %d\n" register

let get_register t reg =
  let open U16 in
  let res = match reg with
  | 2 -> (* Status register *)
    t.memory.latch <- true;
    let r =
      (int_of_bool t.status.vblank_enabled) lsl 7 lor
      (int_of_bool t.status.sprite_0_hit) lsl 6 in
    t.status.vbl_read <- true;
    t.status.vblank_enabled <- false; (u8 r)
  | 4 -> (* OAM data *)
    t.oam.primary.(U8.to_int t.oam.address)
  | 7 -> (* PPU data *)
    (* Palette mirroring *)
    let addr = Mem.address_indirection t.memory t.memory.address in
    increment_ppu_address t;
    (* Correct buffer *)
    if addr >= 0x3F00U then begin
      (* TODO what is this ? why are we doing this ? *)
      t.vram_buffer <- Mem.get t.memory (addr $& 0x2F1FU);
      Mem.get t.memory addr
    end else begin
      let old = t.vram_buffer in
      t.vram_buffer <- Mem.get t.memory addr;
      old
    end
  | _ -> t.memory.bus_latch
  in t.memory.bus_latch <- res ; res

let dma t read cpu_begin =
  let t = t.oam in
  let rec aux cpu_addr oam_addr length =
    if length > 0 then (
      t.primary.(U8.to_int oam_addr) <- read cpu_addr;
      aux U16.(succ cpu_addr) U8.(succ oam_addr) (length - 1)
    )
  in aux cpu_begin t.address 0x100

module R = struct
  let draw_pixel t disp x y pal_start ~pal:palette_nb ~pat:color_nb =
    if color_nb <> 0u then
      (* Get a palette address, a palette number and a color number, give the
       * corresponding color *)
      let address =
        U16.(pal_start + (u16of8 palette_nb) * 4U + (u16of8 color_nb)) in
      let color = Mem.get t.memory address in
      Display.set_pixel Gui.(disp.display) ~x:(U8.to_int x) ~y:(U8.to_int y) ~color

  let copy_bits_16 ~src ~dst mask =
    let open U16 in
    let to_set = src $& mask in
    let with_hole = dst $& ?~mask in
    to_set $| with_hole

  let inc_hori old =
    let v = ref old in
    let open U16 in
    let coarse = !v $& 0x001FU in
    if coarse = 0x1FU then (
      v := !v $& ?~0x1FU;
      v := !v $^ 0x0400U;
    )
    else (v := !v + 1U);
    !v

  let inc_vert old =
    let v = ref old in
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
    ); !v

  let pat_address t ~bank ~offset =
    let open U16 in
    let v = t.memory.address in
    let offset = offset * 16U in
    let finey = (v $& 0x7000U) $>> 12 in
    bank + offset + finey

  let fetch_next_data t =
    let r = t.rendering in
    (* Step number in the fetching process *)
    let local_step = r.cycle mod 8 in
    let open U16 in
    (* Every 8 cycles (9, 17, 25, ..., 257 *)
    (* Actual memory fetching *)
    (* Only 12 first bits of address should be used *)
    begin match local_step with
      | 0 ->
        t.memory.address <- inc_hori t.memory.address;
        if r.cycle = 256 then (
          t.memory.address <- inc_vert t.memory.address;
        )
      | 1 ->
        if r.cycle <> 0 then (
          (* Reload shifters *)
          r.bg.low <- mk_addr ~lo:r.bg.next_low ~hi:(get_hi r.bg.low) ;
          r.bg.high <- mk_addr ~lo:r.bg.next_high ~hi:(get_hi r.bg.high) ;
          r.at.low_next <- U8.(0x1u $& r.at.next <> 0u);
          r.at.high_next <- U8.(0x2u $& r.at.next <> 0u)
        );
        (* load r.nt byte to shift8_r.nt_next *)
        let v = t.memory.address in
        let tile_address = 0x2000U $| (v $& 0xFFFU) in
        r.nt.next <- Mem.get t.memory tile_address
      | 3 -> (* load r.at byte to shift8_r.at_next *)
        (* stolen from mesen *)
        let v = t.memory.address in
        let addr = 0x23C0U $| (v $& 0x0C00U) $| ((v $>> 4) $& 0x38U)
                   $| ((v $>> 2) $& 0x7U) in
        let data = ?$ (Mem.get t.memory addr) in
        let shift = ?% ((0x04U $& (v $>> 4)) $| (v $& 0x02U)) in
        r.at.next <- U8.(?$$) (0x3U $& (data $>> shift))
      | 5 -> (* load low r.bg tile byte to next_r.bg_low (pr.attern table) *)
        let offset = ?$ (r.nt.next) in
        let bank = t.control.background_pattern_address in
        let addr = pat_address t ~bank ~offset in
        r.bg.next_low <- Mem.get t.memory addr
      | 7 -> (* load high r.bg tile byte to next_r.bg_high *)
        let offset = ?$ (r.nt.next) in
        let bank = t.control.background_pattern_address in
        let addr = pat_address t ~bank ~offset in
        r.bg.next_high <- Mem.get t.memory (addr + 8U)
      | _ -> ()
    end

  let render_pixel t disp =
    let r = t.rendering in
    let x = U8.of_int (r.cycle - 1) in
    let y = U8.of_int r.scanline in
    let pat, pal = if t.graphics.background then
      let scroll1 = 15 - U8.to_int t.fine_x_scroll in
      let scroll2 = 7 - U8.to_int t.fine_x_scroll in
      let open U16 in
      let patl = (r.bg.low $>> scroll1) $& 0x1U in
      let path = (r.bg.high $>> scroll1) $& 0x1U in
      let pat = U8.(?$$) (patl $| (path $<< 1)) in
      let open U8 in
      let pal = (((r.at.high $>> scroll2) $<< 1)
                 $| ((r.at.low $>> scroll2) $& 1u)) $& 3u
      in
      (pat, pal)
      else (0u, 0u)
    in
    let spat, spal, priority, sprite_0 =
      if t.graphics.sprites then OAM.pixel t.oam else (0u, 0u, false, false)
    in
    let draw = draw_pixel t disp x y in
    match U8.(?% pat, ?% spat, priority) with
    | _, 0, _ -> draw 0x3F00U ~pal ~pat
    | 0, _, _ -> draw 0x3F10U ~pal:spal ~pat:spat
    | _, _, false ->
      if sprite_0 then (t.status.sprite_0_hit <- true);
      draw 0x3F10U ~pal:spal ~pat:spat
    | _, _, true ->
      if sprite_0 then (t.status.sprite_0_hit <- true);
      draw 0x3F00U ~pal ~pat

  let shift_registers t =
    let r = t.rendering in
    r.bg.low <- U16.(r.bg.low $<< 1);
    r.bg.high <- U16.(r.bg.high $<< 1);
    r.at.low <- U8.(r.at.low $<< 1);
    r.at.high <- U8.(r.at.high $<< 1);
    r.at.low <- if r.at.low_next then U8.(r.at.low $| 1u) else r.at.low;
    r.at.high <- if r.at.high_next then U8.(r.at.high $| 1u) else r.at.high;
    for i = 0 to t.oam.last_sprite_this_scanline do
      (* sprite is active *)
      if t.oam.counters.(i) = 0u then (
        let low, high = t.oam.render_shifters.(i) in
        low := U8.(!low $<< 1);
        high := U8.(!high $<< 1);
      )
    done

  let data_fetching t disp render =
    let r = t.rendering in
    (* Cycle 0 : IDLE *)
    if r.cycle = 0 then ()
    (* Cycles 1 - 256 : BACKGROUND FETCHING *)
    else if r.cycle <= 256 && Graphics.is_rendering t.graphics then (
      fetch_next_data t;
      (* Pixel rendering *)
      if render && (r.cycle > 8 || t.graphics.background_leftmost) then (
        render_pixel t disp
      );
      shift_registers t;
      OAM.decrease_sprite_counters t.oam
    )
    (* Cycles 257 - 320 : NEXT SPRITES FETCHING in another function *)
    else if r.cycle <= 320 then (
      (* If rendering is enabled, the PPU copies all bits related to
       * horizontal position from t to v *)
      (* v: ....A.. ...BCDEF <- t: ....A.. ...BCDEF *)
      if r.cycle = 257 && Graphics.is_rendering t.graphics then (
        t.memory.address <-
          copy_bits_16 ~src:t.memory.temp_address ~dst:t.memory.address 0x41FU
      )
    )
    (* Cycles 321 - 336 : NEXT TWO TILES FETCHING *)
    else if r.cycle <= 336 && Graphics.is_rendering t.graphics then (
      fetch_next_data t ;
      shift_registers t
    )
    (* Cycles 337-340 : USELESS *)
    else ()

  let y_in_range t y_pos =
    let offset = if t.control.sprite_size then 16 else 8 in
    let y_pos = U8.to_int y_pos in
    let scanline = t.rendering.scanline in
    y_pos <= scanline && scanline < y_pos + offset

  let sprite_evaluation t =
    let open OAM in
    let o = t.oam in
    let set_next s = o.sm.state <- Sleep s in
    let decide_next () =
      if o.sm.n = 0 then SM.Full
      else if o.next_open_slot < 32 then SM.CopyY
      else SM.OverflowDetection 0
    in
    match o.sm.state with
    | Sleep next -> o.sm.state <- next
    | CopyY ->
      let y_pos = OAM.get_byte o o.sm.n 0 in
      o.secondary.(o.next_open_slot) <- y_pos;
      if y_in_range t y_pos then (
        if o.sm.n = 0 then (o.sprite_0_here <- true);
        o.next_open_slot <- o.next_open_slot + 1;
        set_next (CopyRest 1)
      ) else (
        o.sm.n <- (o.sm.n + 1) land 0x3f;
        set_next (decide_next ())
      )
    | CopyRest m ->
      o.secondary.(o.next_open_slot) <- OAM.get_byte o o.sm.n m;
      o.next_open_slot <- o.next_open_slot + 1;
      if m = 3 then (
        o.sm.n <- (o.sm.n + 1) land 0x3f;
        set_next (decide_next ())
      ) else set_next (CopyRest (m + 1))
    | OverflowDetection _ -> () (* TODO *)
    | Full -> ()

  let fetch_sprite t =
    let o = t.oam in
    let r = t.rendering in
    let get_oam'_byte n m = o.secondary.(4 * n + m) in
    let sn = (r.cycle - 257) / 8 in
    let step = (r.cycle - 257) mod 8 in
    let enabled = sn < (o.next_open_slot / 4) in
    let fetch_tile_8 ~high =
      let y_pos = get_oam'_byte sn 0 in
      let fine_offset = U16.(?@ (r.scanline) - ?$ y_pos) in
      (* Flip vertically *)
      let fine_offset = if nth_bit o.latches.(sn) 7 then
          U16.(7U - fine_offset)
        else fine_offset
      in
      let index = get_oam'_byte sn 1 in
      let bank = t.control.sprite_pattern_address in
      let open U16 in
      let offset = of_uint8 index in
      let offset = offset * 16U in
      let addr = bank + offset + fine_offset in
      let addr = if high then U16.(addr + 8U) else addr in
      let data = Mem.get t.memory addr in
      (* Flip horizontally *)
      if nth_bit o.latches.(sn) 6 then reverse_byte data else data
    in
    let fetch_tile_16 ~high =
      let y_pos = get_oam'_byte sn 0 in
      let row = r.scanline - U8.(?% y_pos) in
      let vert_flip = nth_bit o.latches.(sn) 7 in
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
      let data = Mem.get t.memory addr in
      (* Flip horizontally *)
      if nth_bit o.latches.(sn) 6 then reverse_byte data else data
    in
    let fetch = if t.control.sprite_size then fetch_tile_16 else fetch_tile_8 in
    match step with
    | 2 -> o.latches.(sn) <- get_oam'_byte sn 2
    | 3 -> o.counters.(sn) <- get_oam'_byte sn 3
    | 4 when enabled -> (fst o.render_shifters.(sn)) := fetch ~high:false
    | 6 when enabled -> (snd o.render_shifters.(sn)) := fetch ~high:true
    | 4 -> (fst o.render_shifters.(sn)) := 0u
    | 6 -> (fst o.render_shifters.(sn)) := 0u
    | _ -> ()

  let sprite_fetching t =
    let r = t.rendering in
    let o = t.oam in
    (* Cycle 0: IDLE *)
    if r.cycle = 0 then ()
    (* Cycles 1-64: clear OAM' *)
    else if r.cycle <= 64 && r.cycle mod 2 = 0 then
      o.secondary.((r.cycle - 1) / 2) <- 0xFFu
    (* Cycles 65-256: sprite evaluation *)
    else if r.cycle <= 256 then (
      if r.cycle = 65 then (
        o.sm.state <- CopyY;
        o.next_open_slot <- 0;
        o.sm.n <- 0;
        o.sprite_0_here <- false;
      );
      sprite_evaluation t;
    )
    (* Cycles 257-320: sprite tile fetching *)
    else if r.cycle <= 320 then (
      o.last_sprite_this_scanline <- o.next_open_slot / 4 - 1;
      fetch_sprite t
    )
    (* Cycles 321-340: IDLE *)
    else ()

  let pre_rendering t disp =
    let r = t.rendering in
    (* Clear VBLANK *)
    if r.cycle = 1 then (
      t.status.vblank_enabled <- false
    );
    (* Fetch data for next frame *)
    sprite_fetching t;
    data_fetching t disp false;
    (*  If rendering is enabled, at the end of vblank, shortly after
     *  the horizontal bits are copied from t to v at dot 257, the PPU
     *  will repeatedly copy the vertical bits from t to v from dots
     *  280 to 304, completing the full initialization of v from t: *)
    if r.cycle >= 280 && r.cycle <= 304 && Graphics.is_rendering t.graphics then (
      t.memory.address <-
        copy_bits_16 ~src:t.memory.temp_address ~dst:t.memory.address 0x7BE0U
    )
    (* Final dot : change everything *)
    else if r.cycle = 340 then (
      t.status.sprite_0_hit <- false ; 
      r.frame <- r.frame + 1;
      t.should_render <- `Yes t.memory.main.(0x3F00);
      (* this should be at cycle 1 *)
      (* Odd frame : jump to (0, 0) directly *)
      if (r.frame mod 2) = 1 && Graphics.is_rendering t.graphics then (
        r.scanline <- 0;
        r.cycle <- 0
      )
    )

  let increment_cycle t =
    let r = t.rendering in
    (* Next cycle *)
    r.cycle <- r.cycle + 1;
    if r.cycle = 341 then (
      r.cycle <- 0;
      (* Next scanline *)
      r.scanline <- r.scanline + 1;
      if r.scanline = 262 then (
        (* End of frame *)
        r.scanline <- 0;
      )
    ) ;
    t.status.vbl_read <- false

  let next_cycle t disp =
    let r = t.rendering in
    (* Process *)
    (* Visible scanlines : 0 - 239 *)
    if r.scanline <= 239 then (
      sprite_fetching t;
      data_fetching t disp true;
    )
    (* Post-render scanline : 240  (IDLE) *)
    else if r.scanline = 240 then ()
    (* Vertical blanking *)
    else if r.scanline = 241 && r.cycle = 1 then (
      if not t.status.vbl_read then t.status.vblank_enabled <- true;
      if t.control.nmi_enabled && not t.status.vbl_read then
        C6502.NMI.pull t.nmi
    )
    (* Last vertical blanking lines : IDLE *)
    else if r.scanline <= 260 then ()
    (* Pre-rendering scanline *)
    else (pre_rendering t disp);
    increment_cycle t
end

let should_render t =
  let old = t.should_render in
  t.should_render <- `No;
  old

let next_cycle = R.next_cycle

module Debug = struct
  type t = {
    names : Display.t;
    attributes : Display.t;
    patterns : Display.t;
    mutable counter : int;
  }

  let palette = Ppu_display.palette
  let pal_4 = [0x000000; 0xFF0000; 0x00FF00; 0x0000FF]

  let create () = {
    names = Display.create ~width:64 ~height:62 ~scale:8 ~palette "Name tables + palettes";
    attributes = Display.create ~width:32 ~height:32
        ~scale:16 ~palette:pal_4 "Attribute tables";
    patterns = Display.create ~width:256 ~height:128
        ~scale:4 ~palette:pal_4 "Pattern tables";
    counter = 0;
  }

  let delete t =
    Display.delete t.names;
    Display.delete t.attributes;
    Display.delete t.patterns

  let render_nametables t disp =
    let set_nametable addr x_orig y_orig =
      for y = 0 to 29 do
        for x = 0 to 31 do
          let addr = addr + y * 32 + x in
          let v = Mem.get t.memory (U16.of_int addr) in
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
      let color = t.memory.main.(0x3F00 + x) in
      Display.set_pixel disp ~x:(x * 2) ~y:61 ~color;
      Display.set_pixel disp ~x:(x * 2 + 1) ~y:61 ~color
    done

  let render_attributes t disp =
    let set_attr addr x_orig y_orig =
      for y = 0 to 7 do
        for x = 0 to 7 do
          let addr = addr + y * 8 + x in
          let v = t.memory.main.(addr) in
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

  let render_patterns t disp =
    let set_pattern addr x_orig y_orig =
      for tile_y = 0 to 15 do
        for tile_x = 0 to 15 do
          let addr = addr + (tile_y * 16 + tile_x) * 16 in
          for y = 0 to 7 do
            for x = 0 to 7 do
              let low = t.memory.main.(addr + y) in
              let high = t.memory.main.(addr + y + 8) in
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

  let cooldown = 1000000

  let render t = function
    | None -> ()
    | Some ({names; attributes; patterns; _} as s) ->
      if s.counter = 0 then (
        render_nametables t names;
        render_attributes t attributes;
        render_patterns t patterns;
        Display.render names;
        Display.render attributes;
        Display.render patterns;
        s.counter <- cooldown
      );
      s.counter <- s.counter - 1
end
