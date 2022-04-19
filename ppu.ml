open Stdint

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

(* Main memory *)
let memory = Array.make 0x4000 0u
let oam = Array.make 0x100 0u

(* Status *)
let vblank_enabled = ref true

(* Control register *)
let ppudata_increment = ref 1U
let sprite_pattern_address = ref 0x0000U
let background_pattern_address = ref 0x0000U
let sprite_size = ref false
let master_slave_mode = ref false
let nmi_enabled = ref false

(* Mask register *)
let greyscale = ref false
let show_background_leftmost = ref false
let show_sprites_leftmost = ref false
let show_background = ref false
let show_sprites = ref false
let emph_red = ref false
let emph_green = ref false
let emph_blue = ref false

let mirroring_mode = ref false (* 0 : horizon (vert arr) *)
let sprite_0_hit = ref false

let oam_address = ref 0u
let ppu_address = ref 0U

(* Scrolling *)
let temp_vram_address = ref 0U (* 15-bit *)
let fine_x_scroll = ref 0u

let interrupt_cpu = ref None
let vbl_read = ref false

(* Latch for PPUSCROLL and PPUADDR *)
let latch = ref true (* True : first set *)
let read_latch () =
  let r = !latch in
  latch := not !latch;
  r

let bus_latch = ref 0x00u

let int_of_bool b = if b then 1 else 0
let nth_bit b n =
  Uint8.((logand b (shift_left one n)) <> zero)

let palette_mirror_filter addr =
  if addr >= 0x3F00U then (
    let tmp = Uint16.logand !ppu_address 0x3F1FU in
    if tmp = 0x3F10U then 0x3F00U else tmp
  )
  else addr

let increment_ppu_address () = ppu_address := Uint16.(!ppu_address + !ppudata_increment)

open C6502.Int_utils
let set_register register (v : uint8) =
  bus_latch := v ;
  match register with
  | 0 -> (* Control register *)
    (* t: ...GH.. ........ <- d: ......GH *)
    let to_set = Uint16.shift_left (u16of8 @@ Uint8.logand v 3u) 10 in
    let with_hole = Uint16.logand !temp_vram_address 0b111001111111111U in
    temp_vram_address := Uint16.logor to_set with_hole ;

    (* TODO going accross, going down ? *)
    ppudata_increment := if nth_bit v 2 then 32U else 1U;
    sprite_pattern_address := if (nth_bit v 3) then 0x1000U else 0U;
    background_pattern_address := if (nth_bit v 4) then 0x1000U else 0U;
    sprite_size := nth_bit v 5;
    master_slave_mode := nth_bit v 6;
    nmi_enabled := nth_bit v 7
  | 1 -> (* Mask register *)
    greyscale := nth_bit v 0;
    show_background_leftmost := nth_bit v 1;
    show_sprites_leftmost := nth_bit v 2;
    show_background := nth_bit v 3;
    show_sprites := nth_bit v 4;
    emph_red := nth_bit v 5;
    emph_green := nth_bit v 6;
    emph_blue := nth_bit v 7
  | 3 -> (* OAM address *)
    oam_address := v
  | 4 -> (* OAM data *)
    oam.(Uint8.to_int !oam_address) <- v;
    oam_address := Uint8.(succ !oam_address)
  | 5 -> (* Scroll register *)
    if read_latch () then
      (* t: ....... ...ABCDE <- d: ABCDE... *)
      let to_set = Uint16.shift_right_logical (u16of8 v) 3 in
      let with_hole = Uint16.logand !temp_vram_address 0b111111111100000U in
      temp_vram_address := Uint16.logor to_set with_hole ;
      (* x:              FGH <- d: .....FGH *)
      fine_x_scroll := Uint8.logand v 0b00000111u
    else
      (* t: FGH..AB CDE..... <- d: ABCDEFGH *)
      let fgh = Uint16.shift_left (u16of8 (Uint8.logand v 3u)) 13 in
      let abcde = Uint16.shift_left (u16of8 (Uint8.logand v 0b11111000u)) 2 in
      let with_hole = Uint16.logand !temp_vram_address 0b000110000011111U in
      let with_fgh = Uint16.logor with_hole fgh in
      temp_vram_address := Uint16.logor with_fgh abcde
  | 6 -> (* PPU address *)
    if read_latch () then
      (* t: .CDEFGH ........ <- d: ..CDEFGH
         <unused>     <- d: AB......
         t: Z...... ........ <- 0 (bit Z is cleared) *)
      temp_vram_address := Uint16.logand
          (mk_addr ~lo:(get_lo !temp_vram_address) ~hi:v) 0x3FFFU
    else (
      (* t: ....... ABCDEFGH <- d: ABCDEFGH
         v: <...all bits...> <- t: <...all bits...> *)
      temp_vram_address := mk_addr ~hi:(get_hi !temp_vram_address) ~lo:v ;
      ppu_address := !temp_vram_address
    )
  | 7 -> (* PPU data *)
    (* Palette mirroring *)
    let addr = palette_mirror_filter !ppu_address in
    memory.(Uint16.to_int addr) <- v;
    increment_ppu_address ()
  | _ -> Printf.printf "Warning: trying to set PPU register %d\n" register

let vram_buffer = ref 0u
let get_register reg =
  let res = match reg with
  | 2 -> (* Status register *)
    latch := true;
    let r =
      (int_of_bool !vblank_enabled) lsl 7 lor
      (int_of_bool !sprite_0_hit) lsl 6 in
    vbl_read := true;
    vblank_enabled := false; (u8 r)
  | 4 -> (* OAM data *)
    oam.(Uint8.to_int !oam_address)
  | 7 -> (* PPU data *)
    (* Palette mirroring *)
    let addr = palette_mirror_filter !ppu_address in
    ppu_address := Uint16.(!ppu_address + !ppudata_increment);
    (* Correct buffer *)
    if addr >= 0x3F00U then begin
      vram_buffer := Uint16.(memory.(to_int @@ logand addr 0x2F1FU));
      memory.(Uint16.to_int addr)
    end else begin
      let old = !vram_buffer in
      vram_buffer := memory.(Uint16.to_int addr); old
    end
  | _ -> !bus_latch
  in bus_latch := res ; res

let dma read cpu_begin =
  let rec aux cpu_addr oam_addr length =
    if length > 0 then (
      oam.(Uint8.to_int oam_addr) <- read cpu_addr;
      aux (Uint16.succ cpu_addr) (Uint8.succ oam_addr) (length - 1)
    )
  in aux cpu_begin !oam_address 0x100

[@@@warning "-32"]

module Rendering = struct
  let frame = ref 0
  let scanline = ref 261
  let cycle = ref 0

  (* For storing pattern table data for two tiles *)
  let bg_low, bg_high = ref 0U, ref 0U (* low, high *)
  let next_bg_low = ref 0u
  let next_bg_high = ref 0u
  (* For storing palette attributes, not shifting *)
  let at, at_next = ref 0u, ref 0u
  let nt_next = ref 0u

  (* Internal rendering registers : SPRITES
   * Holds 8 sprites and their pattern table
   * data, attributes and positions for the scanline *)
  let sec_oam = Array.make 0x20 0u
  let sprites_shifts = Array.make 16 0u
  let sprite_attributes = Array.make 8 0u
  let sprite_positions = Array.make 8 0u

  let decode_chr (start : uint16) (tile_nb : uint8) (x : uint8) (y : uint8) =
    let chr_base = Uint16.(start + (u16of8 tile_nb) * 0x10U) in
    let x_mod = u16of8 @@ Uint8.logand x 7u in
    let y_mod = u16of8 @@ Uint8.logand y 7u in
    let low_byte = memory.(Uint16.(to_int @@ chr_base + y_mod)) in
    let high_byte = memory.(Uint16.(to_int @@ chr_base + 8U + y_mod)) in
    let mask = Uint8.shift_left 1u (7 - (Uint16.to_int x_mod)) in
    let low_bit = int_of_bool Uint8.(logand low_byte mask != zero) in
    let high_bit = int_of_bool Uint8.(logand high_byte mask != zero) in
    (u8 @@ low_bit lor (high_bit lsl 1))

  (*
  let get_address (x : uint16) (y : uint16) =
    let open Uint16 in
    let x_add = x + (u16 32)
                            * (u16of8 Uint8.(logand !base_nametable one)) in
    let y_add = x + (u16 30) (* TODO : why 30 ? *)
                            * (u16of8 Uint8.(shift_right_logical !base_nametable 1)) in
    let x_mir = rem x_add (u16 (if !mirroring_mode then 64 else 32)) in
    let y_mir = rem y_add (u16 (if !mirroring_mode then 30 else 60)) in
    let quad_nb = (y_mir / (u16 30)) * (u16 2) + (x_mir / (u16 32)) in
    let base = (u16 0x2000) + (u16 0x400) * quad_nb in
    base, (rem x (u16 32)), (rem y (u16 30))
     *)

  (* Get a palette address, a palette number and a color number, give the
   * corresponding color *)
  let palette_ind_to_color start nb ind =
    let address = Uint16.(start + (u16of8 nb) * 4U + (u16of8 ind)) in
    memory.(Uint16.to_int address)

  let sprite_warned = ref false

  let draw_pixel disp x y pal_start ~pal:palette_nb ~pat:color_nb =
    if color_nb <> 0u then
      let color = palette_ind_to_color pal_start palette_nb color_nb in
      Display.set_pixel disp ~x:(Uint8.to_int x) ~y:(Uint8.to_int y) ~color

  let render_sprite nb f =
    if !sprite_size && not !sprite_warned then (
      Printf.printf "Unsupported 8x16 sprites\n";
      sprite_warned := true
    ) ;
    let ypos = oam.(nb) in
    let xpos = oam.(nb + 3) in
    let attributes = oam.(nb + 2) in
    let tile_nb = oam.(nb + 1) in
    let palette = Uint8.logand attributes 3u in
    let flip_h = nth_bit attributes 6 in
    let flip_v = nth_bit attributes 7 in
    for y = 0 to 7 do
      if Uint8.(ypos + (u8 y)) < 240u then
        for x = 0 to 7 do
          let x' = Uint8.(xpos + (u8 x)) in
          let y' = Uint8.(ypos + (u8 y)) in
          let fx = if flip_h then 7 - x else x in
          let fy = if flip_v then 7 - y else y in
          let color_nb = decode_chr !sprite_pattern_address
              tile_nb (u8 fx) (u8 fy) in
          f x' y' 0x3F10U ~pal:palette ~pat:color_nb
        done
    done

  let rec render_sprites disp after_back nb =
    if nb != 256 then (
      if (Uint8.logand oam.(nb + 2) 0x20u <> 0u) <> after_back then
        render_sprite nb (draw_pixel disp)
      ;
      render_sprites disp after_back (nb + 4)
    )

  let get_sprite_zero_pixels () =
    let pixels = ref [] in
    let fill x y _ ~pal:_ ~pat:color =
      if color <> 0u then pixels := (x, y) :: !pixels
    in
    (* TODO sprite 0 should be dependent on OAM addr *)
    render_sprite 0 fill ; !pixels

  let sprite_zero_check x y bg_color =
    match bg_color with
    | None -> ()
    | Some _ ->
      if !show_background && !show_sprites then
        (* compute these only once per frame *)
        let pixels = get_sprite_zero_pixels () in
        begin match List.find_opt ((=) (x, y)) pixels with
          | None -> ()
          | Some _ -> sprite_0_hit := !sprite_0_hit || true
        end

  let shift1_8 r = r := Uint8.shift_left !r 1
  let shift1_16 r = r := Uint16.shift_left !r 1

  (* Remember:
   * - name table: associate a kind of pattern to a tile
   * - attribute table (depends on chosen name table): associate a palette to a tile
   * - pattern table: defines different patterns
   *)

  let inc_hori v =
    let open Uint16 in
    let coarse = logand !v 0x001FU in
    if coarse = 0x1FU then (
      v := logand !v (lognot 0x1FU);
      v := logxor !v 0x0400U;
    )
    else v := !v + 1U

  let inc_vert v =
    let open Uint16 in
    if (logand !v 0x7000U) <> 0x7000U then
      v := !v + 0x1000U
    else (
      v := logand !v (lognot 0x7000U);
      let y = ref (shift_right_logical (logand !v 0x03E0U) 5) in
      if !y = 29U then (
        y := 0U;
        v := logxor !v 0x0800U
      )
      else if !y = 31U then (
        y := 0U
      )
      else (
        y := !y + 1U
      ) ;
      v := logor (logand !v (lognot 0x03E0U)) (shift_left !y 5)
    )

  let fetch_next_data () =
    (* Step number in the fetching process *)
    let local_step = !cycle mod 8 in
    (* Every 8 cycles (9, 17, 25, ..., 257 *)
    (* Actual memory fetching *)
    (* Only 12 first bits of address should be used *)
    begin match local_step with
      | 0 when !cycle = 0 ->
        (*Printf.printf "addr %X scanline %d\n"
          (Uint16.to_int !ppu_address) !scanline*) ()
      | 0 when !cycle <> 0 ->
        if !cycle = 256 then inc_vert ppu_address
        else inc_hori ppu_address
      | 1 ->
        if !cycle <> 0 then (
          (* Reload shifters *)
          bg_low := mk_addr ~lo:!next_bg_low ~hi:(get_hi !bg_low) ;
          bg_high := mk_addr ~lo:!next_bg_high ~hi:(get_hi !bg_high) ;
          at := Uint8.(logand !at_next 0x3u);
        );
        (* load NT byte to shift8_nt_next *)
        let open Uint16 in
        let v = !ppu_address in
        let tile_address = logor 0x2000U (logand v 0xFFFU) in
        (* TODO mirroring *)
        nt_next := memory.(Uint16.to_int tile_address)
      | 3 ->
        (* load AT byte to shift8_at_next
           The low 12 bits of the attribute address are composed in the following way:
           NN 1111 YYY XXX
           || |||| ||| +++-- high 3 bits of coarse X (x/4)
           || |||| +++------ high 3 bits of coarse Y (y/4)
           || ++++---------- attribute offset (960 bytes)
           ++--------------- nametable select *)
        let open Uint16 in
        let v = !ppu_address in
        let attribute_base =0x23C0U + (logand v 0x0C00U) in
        let coarse_y = logand 0x38U (shift_right_logical v 4) in
        let coarse_x = logand 0x07U (shift_right_logical v 2) in
        let attribute_address = attribute_base + coarse_x + coarse_y in
        at_next := memory.(to_int attribute_address) 
      | 5 -> (* load low BG tile byte to next_bg_low (pattern table)
                PPU addresses within the pattern tables can be decoded as follows:
                0HRRRR CCCCPTTT
                |||||| |||||+++- T: Fine Y offset, the row number within a tile
                |||||| ||||+---- P: Bit plane (0: "lower"; 1: "upper")
                |||||| ++++----- C: Tile column
                ||++++---------- R: Tile row
                |+-------------- H: Half of sprite table (0: "left"; 1: "right")
                +--------------- 0: Pattern table is at $0000-$1FFF *)
        let open Uint16 in
        let v = !ppu_address in
        let finey = shift_right_logical (logand v 0x7000U) 12 in
        let tile_offset = (of_uint8 !nt_next * 16U) in
        let addr = !background_pattern_address + tile_offset + finey in
        (* reverse byte, for some reason *)
        next_bg_low := memory.(to_int addr);
      | 7 ->
        (* load high BG tile byte to next_bg_high
         * same as above with additional bit *)
        let open Uint16 in
        let finey = shift_right_logical (logand !ppu_address 0x7000U) 12 in
        let tile_offset = (of_uint8 !nt_next * 16U)  in
        let addr = !background_pattern_address + tile_offset + finey + 8U in
        next_bg_high := memory.(to_int addr);
      | _ -> ()
    end

  let render_pixel disp =
    let scroll = 15 - Uint8.to_int !fine_x_scroll in
    let open Uint16 in
    let patl = logand (shift_right !bg_low scroll) 0x1U in
    let path = logand (shift_right !bg_high scroll) 0x1U in
    let pat = logor (shift_left path 1) patl |> Uint16.to_uint8 in
    let open Uint8 in
    let pal = !at in
    let x = of_int !cycle in
    let y = of_int !scanline in
    draw_pixel disp x y 0x3F00U ~pal ~pat

  let data_fetching disp render =
    (* Cycles 0 - 256 : BACKGROUND FETCHING *)
    if !cycle <= 256 && !show_background then (
      fetch_next_data ();
      (* Pixel rendering *)
      if render then (render_pixel disp);
      (* Shift registers *)
      shift1_16 bg_low ;
      shift1_16 bg_high
    )
    (* Cycles 257 - 320 : NEXT SPRITES FETCHING *)
    else if !cycle <= 320 then (
      (* If rendering is enabled, the PPU copies all bits related to
       * horizontal position from t to v *)
      (* v: ....A.. ...BCDEF <- t: ....A.. ...BCDEF *)
      if !cycle = 257 && !show_background then (
        let mask = 0x41FU in
        let to_set = Uint16.logand !temp_vram_address mask in
        let with_hole = Uint16.logand !ppu_address (Uint16.lognot mask) in
        ppu_address := Uint16.logor to_set with_hole
      )
      (* TODO sprites *)
    )
    (* Cycles 321 - 336 : NEXT TWO TILES FETCHING *)
    else if !cycle <= 336 && !show_background then (
      fetch_next_data () ;
      shift1_16 bg_low ;
      shift1_16 bg_high
    )
    (* Cycles 337-340 : USELESS *)
    else ()

  let next_cycle disp =
    (* Process *)
    (* Visible scanlines : 0 - 239 *)
    if !scanline <= 239 then (
      data_fetching disp true
    )
    (* Post-render scanline : 240  (IDLE) *)
    else if !scanline = 240 then ()
    (* Vertical blanking *)
    else if !scanline = 241 && !cycle = 1 then (
      if not !vbl_read then vblank_enabled := true;
      if !nmi_enabled && not !vbl_read then
        Option.get !interrupt_cpu ()
    )
    (* Last vertical blanking lines : IDLE *)
    else if !scanline <= 260 then ()
    (* Pre-rendering scanline *)
    else (
      (* Fetch data for next frame *)
      data_fetching disp false;
      (*  If rendering is enabled, at the end of vblank, shortly after
       *  the horizontal bits are copied from t to v at dot 257, the PPU
       *  will repeatedly copy the vertical bits from t to v from dots
       *  280 to 304, completing the full initialization of v from t: *)
      if !cycle >= 280 && !cycle <= 304 && !show_background then (
          let mask = 0x7BE0U in
          let to_set = Uint16.logand !temp_vram_address mask in
          let with_hole = Uint16.logand !ppu_address (Uint16.lognot mask) in
          ppu_address := Uint16.logor to_set with_hole
      )
      (* Final dot : change everything *)
      else if !cycle = 340 then (
        sprite_0_hit := false ; 
        incr frame ;
        (*render_sprites true 0 ;*)
        Display.render disp;
        Display.clear disp memory.(0x3F00);
        (*render_sprites false 0 ;*)
        (* this should be at cycle 1 *)
        vblank_enabled := false ;
        (* Odd frame : jump to (0, 0) directly *)
        if (!frame mod 2) = 1 && !show_background then (
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
    vbl_read := false
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
          let v = memory.(addr) in
          Display.set_pixel disp ~x:(x + x_orig) ~y:(y + y_orig) ~color:v
        done
      done
    in
    set_nametable 0x2000 0 0;
    set_nametable 0x2400 32 0;
    set_nametable 0x2800 0 30;
    set_nametable 0x2C00 32 30;
    for x = 0 to 63 do
      Display.set_pixel disp ~x ~y:60 ~color:(Uint8.of_int x)
    done;
    for x = 0 to 31 do
      let color = memory.(0x3F00 + x) in
      Display.set_pixel disp ~x:(x * 2) ~y:61 ~color;
      Display.set_pixel disp ~x:(x * 2 + 1) ~y:61 ~color
    done

  let render_attributes disp =
    let set_attr addr x_orig y_orig =
      for y = 0 to 7 do
        for x = 0 to 7 do
          let addr = addr + y * 8 + x in
          let v = memory.(addr) in
          let x' = x * 2 + x_orig in
          let y' = y * 2 + y_orig in
          let open Uint8 in
          let top_left = logand 0x3u v in
          let top_right = logand (shift_right_logical v 2) 0x3u in
          let bot_left = logand (shift_right_logical v 4) 0x3u in
          let bot_right = logand (shift_right_logical v 6) 0x3u in
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
              let low = memory.(addr + y) in
              let high = memory.(addr + y + 8) in
              let shift = 7 - x in
              let open Uint8 in
              let low = logand (shift_right_logical low shift) 0x1u in
              let high = logand (shift_right_logical high shift) 0x1u in
              let color = logor low (high * 2u) in
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
