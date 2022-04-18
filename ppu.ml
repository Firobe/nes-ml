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

let dump_memory () =
  let file = open_out_bin "memdump_vram" in
  let store = Bytes.create 0x10000 in
  for i = 0 to (Array.length memory) - 1 do
    Bytes.set store i @@ char_of_int (Uint8.to_int memory.(i))
  done ;
  output file store 0 (Bytes.length store) ;
  close_out file

[@@@warning "-32"]

module Rendering = struct
  let frame = ref 0
  let scanline = ref 261
  let cycle = ref 0

  (* For storing pattern table data for two tiles *)
  let shift16_1, shift16_2 = ref 0U, ref 0U (* low, high *)
  let shift16_latch_low = ref 0u
  let shift16_latch_high = ref 0u
  (* For storing palette attributes, not shifting *)
  let at = ref 0u
  let at_latch, nt_latch = ref 0u, ref 0u

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

  (*
  let render_background_pixel (x : uint16) (y : uint16) =
    let x_tile = Uint16.shift_right_logical x 3 in
    let y_tile = Uint16.shift_right_logical y 3 in
    let base_addr, x_mod, y_mod = get_address x_tile y_tile in
    let address = Uint16.(base_addr + y_mod * (u16 32) + x_mod) in
    let tile_kind = memory.(Uint16.to_int @@ address) in
    let color_nb = decode_chr !background_pattern_address tile_kind (u8of16 x)
        (u8of16 y) in
    if color_nb = (u8 0) then None
    else
      (* Decode attribute table *)
      let attr_table_address = Uint16.(base_addr + (u16 0x3C0)) in
      let x_big = Uint16.shift_right_logical x_mod 2 in
      let y_big = Uint16.shift_right_logical y_mod 2 in
      let big_addr = Uint16.(attr_table_address + y_big * (u16 8) + x_big) in
      let big_byte = memory.(Uint16.to_int @@ big_addr) in
      let block_offset =
        ((((Uint16.to_int x_mod) / 2) mod 2) + 2 * (((Uint16.to_int y_mod) / 2) mod 2)) * 2 in
      let palette_nb = 
        Uint8.(logand (shift_right_logical big_byte block_offset) (u8 0x3)) in
      (* Get palette *)
      Some (palette_ind_to_color (u16 0x3F00) palette_nb color_nb)
     *)

  let sprite_warned = ref false

  let draw_pixel x y pal_start ~pal:palette_nb ~pat:color_nb =
    if color_nb <> 0u then
      let color = palette_ind_to_color pal_start palette_nb color_nb in
      Display.set_pixel (Uint8.to_int x) (Uint8.to_int y) color


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

  let rec render_sprites after_back nb =
    if nb != 256 then (
      if (Uint8.logand oam.(nb + 2) 0x20u <> 0u) <> after_back then
        render_sprite nb draw_pixel
      ;
      render_sprites after_back (nb + 4)
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

  let shift1_8 r = r := Uint8.shift_right_logical !r 1
  let shift1_16 r = r := Uint16.shift_right_logical !r 1

  (* Remember:
   * - name table: associate a kind of pattern to a tile
   * - attribute table (depends on chosen name table): associate a palette to a tile
   * - pattern table: defines different patterns
   *)

  let next_cycle () =
    (* TODO increment vert address at cycle 256 *)
    (* Process *)
    (* Visible scanlines : 0 - 239 *)
    if !scanline <= 239 then (
      (* Cycle 0 : IDLE *)
      if !cycle = 0 then ()
      (* Cycles 1 - 256 : BACKGROUND FETCHING *)
      else if !cycle <= 256 && !show_background then (
        (* Step number in the fetching process *)
        let local_step = (!cycle - 1) mod 8 in
        (* Every 8 cycles (9, 17, 25, ..., 257 *)
        if !cycle >= 9 && local_step = 0 then (
          (* Reload shifters *)
          (* TODO process AT and NT to store palette attributes *)
          (*Printf.printf "%x\n%!" (Uint8.to_int !shift16_latch_high);*)
          shift16_1 := mk_addr ~hi:!shift16_latch_low ~lo:(get_lo !shift16_1) ;
          shift16_2 := mk_addr ~hi:!shift16_latch_high ~lo:(get_lo !shift16_2) ;
          at := Uint8.(logand !at_latch 0x3u)
        ) ;
        (* Actual memory fetching *)
        (* Only 12 first bits of address should be used *)
        match local_step with
        | 1 ->
          (* load NT byte to shift8_nt_latch *)
          let open Uint16 in
          let v = !ppu_address in
          let tile_address = logor 0x2000U (logand v 0xFFFU) in
          nt_latch := memory.(Uint16.to_int tile_address)
        | 3 ->
          (* load AT byte to shift8_at_latch
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
          at_latch := memory.(to_int attribute_address) 
        | 5 -> (* load low BG tile byte to shift16_latch_low (pattern table)
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
          let tile_offset = (of_uint8 !nt_latch * 16U) in
          let addr = !background_pattern_address + tile_offset + finey in
          shift16_latch_low := memory.(to_int addr);
        | 7 ->
          (* load high BG tile byte to shift16_latch_high
           * same as above with additional bit *)
          let open Uint16 in
          let finey = shift_right_logical (logand !ppu_address 0x7000U) 12 in
          let tile_offset = (of_uint8 !nt_latch * 16U)  in
          let addr = !background_pattern_address + tile_offset + finey + 8U in
          shift16_latch_high := memory.(to_int addr);
          (* inc hori *) (* TODO correctly *)
          ppu_address := !ppu_address + 1U
        | _ -> ()
          ;
          (* Pixel rendering *)
        let open Uint16 in
        let scroll = Uint8.to_int !fine_x_scroll in
        let patl = logand (shift_right !shift16_1 scroll) 0x1U in
        let path = logand (shift_right !shift16_2 scroll) 0x1U in
        let pat = logor (shift_left path 1) patl |> Uint16.to_uint8 in
        let open Uint8 in
        let pal = !at in
        let x = of_int !cycle in
        let y = of_int !scanline in
        (*Printf.printf "%d %d\n%!" (Uint8.to_int pat) (Uint8.to_int pal);*)
        (*Printf.printf "x %d y %d (%d, %d)\n" !cycle !scanline (to_int pal)
         * (to_int pat);*)
        draw_pixel x y 0x3F00U ~pal ~pat;
        (* Shift registers *)
        shift1_16 shift16_1 ;
        shift1_16 shift16_2
      )
      (* Cycles 257 - 320 : NEXT SPRITES FETCHING *)
      else if !cycle <= 320 then (
        (* TODO *)
      )
      (* Cycles 321 - 336 : NEXT TWO TILES FETCHING *)
      else if !cycle <= 336 then (
        (* TODO *)
      )
      (* Cycles 337-340 : USELESS *)
      else ()
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
      (*  If rendering is enabled, at the end of vblank, shortly after
       *  the horizontal bits are copied from t to v at dot 257, the PPU
       *  will repeatedly copy the vertical bits from t to v from dots
       *  280 to 304, completing the full initialization of v from t: *)
      if !cycle >= 280 && !cycle <= 304 then (
          let mask = u16 0b111101111100000 in
          let to_set = Uint16.logand !temp_vram_address mask in
          let with_hole = Uint16.logand !ppu_address (Uint16.lognot mask) in
          ppu_address := Uint16.logor to_set with_hole
      )
      (* Final dot : change everything *)
      else if !cycle = 340 then (
        sprite_0_hit := false ;
        incr frame ;
        (*render_sprites true 0 ;*)
        Display.display () ;
        Display.clear_screen memory.(0x3F00);
        (*render_sprites false 0 ;*)
        vblank_enabled := false ;
        (*Printf.printf "frame %d\n%!" !frame;*)
        (* Odd frame : jump to (0, 0) directly *)
        if (!frame mod 2) = 1 && !show_background then (
          scanline := 0;
          cycle := 0
        )
      )
    ) ;
    (* If rendering is enabled, the PPU copies all bits related to
     * horizontal position from t to v *)
    (* v: ....A.. ...BCDEF <- t: ....A.. ...BCDEF *)
    if !cycle = 257 then (
      let mask = 0b000010000011111U in
      let _to_set = Uint16.logand !temp_vram_address mask in
      let _with_hole = Uint16.logand !ppu_address (Uint16.lognot mask) in
      (* TODO FIX, this corrupts PPU address (palette fails to load, for example
       *
      ppu_address := Uint16.logor to_set with_hole*)
      ()
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

[@@@warning "+32"]

(*
let debug_vram scale =
    Graphics.open_graph "";
    Graphics.resize_window (256 * scale) (128 * scale);
    for x = 0 to 31 do
        for y = 0 to 15 do
            for x_loc = 0 to 7 do
                for y_loc = 0 to 7 do
                    let kind = decode_chr 0 (x * 16 + y) x_loc y_loc in
                    let gr = kind * 64 in
                    Graphics.set_color (Graphics.rgb gr gr gr);
                    Graphics.fill_rect (scale * (x * 8 + x_loc))
                        (scale * (y*8 + y_loc)) scale scale;
                done
            done;
            let str = Printf.sprintf "%X" (16*(x * 16 + y)) in
            Graphics.moveto (scale * (x * 8)) (scale * (y*8));
            Graphics.set_color Graphics.red;
            Graphics.draw_string str
        done
    done
*)

let init ic mm =
  interrupt_cpu := Some ic;
  mirroring_mode := mm;
  Display.init ()

let next_cycle = Rendering.next_cycle

let exit () =
  dump_memory ();
  Display.exit ()
