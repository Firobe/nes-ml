open Stdint
open C6502.Int_utils

let memory = Array.make 0x4000 (u8 0)
let oam = Array.make 0x100 (u8 0)

(* Status *)
let vblank_enabled = ref true

(* Scrolling *)
let horizontal_scroll = ref (u8 0)
let vertical_scroll = ref (u8 0)

(* Control register *)
let base_nametable = ref (u8 0x0)
let ppudata_increment = ref (u16 0x1)
let sprite_pattern_address = ref (u16 0x0000)
let background_pattern_address = ref (u16 0x0000)
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

let oam_address = ref (u8 0x0)
let ppu_address = ref (u16 0x0)

let interrupt_cpu = ref None
let vbl_read = ref false

(* Latch for PPUSCROLL and PPUADDR *)
let latch = ref true (* True : first set *)
let read_latch () =
  let r = !latch in
  latch := not !latch;
  r

let bus_latch = ref (u8 0x00)

let int_of_bool b = if b then 1 else 0
let nth_bit b n =
  Uint8.((logand b (shift_left one n)) <> zero)

let palette_mirror_filter addr =
  Printf.printf "%X\n" (Uint16.to_int addr);
  if addr >= (u16 0x3F00) then
    let tmp = Uint16.logand !ppu_address (u16 0x3F1F) in
    if tmp = (u16 0x3F10) then (u16 0x3F00) else tmp
  else addr

let set_register register (v : uint8) =
  bus_latch := v ;
  match register with
  | 0 -> (* Control register *)
    base_nametable := Uint8.logand v (u8 0x3);
    (* TODO going accross, going down ? *)
    ppudata_increment := if nth_bit v 2 then (u16 32) else (u16 1);
    sprite_pattern_address := if (nth_bit v 3) then (u16 0x1000) else (u16 0x0);
    background_pattern_address := if (nth_bit v 4) then (u16 0x1000) else (u16 0x0);
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
      horizontal_scroll := v
    else
      vertical_scroll := v
  | 6 -> (* PPU address *)
    if read_latch () then
      ppu_address := Uint16.logand
          (mk_addr ~lo:(get_lo !ppu_address) ~hi:v) (u16 0x3FFF)
    else
      ppu_address := mk_addr ~hi:(get_hi !ppu_address) ~lo:v | 7 -> (* PPU data *)
    (* Palette mirroring *)
    let addr = palette_mirror_filter !ppu_address in
    memory.(Uint16.to_int addr) <- v;
    ppu_address := Uint16.logand (u16 0x3FFF) Uint16.(!ppu_address + !ppudata_increment)
  | _ -> Printf.printf "Warning: trying to set PPU register %d\n" register

let vram_buffer = ref (u8 0)
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
    if addr >= (u16 0x3F00) then begin
      vram_buffer := Uint16.(memory.(to_int @@ logand addr (u16 0x2F1F)));
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

(*
let dump_memory () =
  let file = open_out_bin "memdump_vram" in
  let store = Bytes.create 0x10000 in
  for i = 0 to (Array.length memory) - 1 do
    Bytes.set store i @@ char_of_int memory.(i)
  done ;
  output file store 0 (Bytes.length store) ;
   close_out file
   *)

module Rendering = struct
  let frame = ref 0
  let scanline = ref 261
  let cycle = ref 0

  let decode_chr (start : uint16) (tile_nb : uint8) (x : uint8) (y : uint8) =
    let chr_base = Uint16.(start + (u16of8 tile_nb) * (u16 0x10)) in
    let x_mod = u16of8 @@ Uint8.logand x (u8 0x7) in
    let y_mod = u16of8 @@ Uint8.logand y (u8 0x7) in
    let low_byte = memory.(Uint16.(to_int @@ chr_base + y_mod)) in
    let high_byte = memory.(Uint16.(to_int @@ chr_base + (u16 0x8) + y_mod)) in
    let mask = Uint8.shift_left (u8 1) (7 - (Uint16.to_int x_mod)) in
    let low_bit = int_of_bool Uint8.(logand low_byte mask != zero) in
    let high_bit = int_of_bool Uint8.(logand high_byte mask != zero) in
    (u8 @@ low_bit lor (high_bit lsl 1))

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

  (* Get a palette address, a palette number and a color number, give the
   * corresponding color *)
  let palette_ind_to_color start nb ind =
    let address = Uint16.(start + (u16of8 nb) * (u16 4) + (u16of8 ind)) in
    memory.(Uint16.to_int address)

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

  let sprite_warned = ref false

  let draw_pixel x y pal_start palette_nb color_nb =
    if color_nb <> (u8 0) then
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
    let palette = Uint8.logand attributes (u8 0x3) in
    let flip_h = nth_bit attributes 6 in
    let flip_v = nth_bit attributes 7 in
    for y = 0 to 7 do
      if Uint8.(ypos + (u8 y)) < (u8 240) then
        for x = 0 to 7 do
          let x' = Uint8.(xpos + (u8 x)) in
          let y' = Uint8.(ypos + (u8 y)) in
          let fx = if flip_h then 7 - x else x in
          let fy = if flip_v then 7 - y else y in
          let color_nb = decode_chr !sprite_pattern_address
              tile_nb (u8 fx) (u8 fy) in
          f x' y' (u16 0x3F10) palette color_nb
        done
    done

  let rec render_sprites after_back nb =
    if nb != 256 then (
      if (Uint8.logand oam.(nb + 2) (u8 0x20) <> (u8 0)) <> after_back then
        render_sprite nb draw_pixel
      ;
      render_sprites after_back (nb + 4)
    )

  let get_sprite_zero_pixels () =
    let pixels = ref [] in
    let fill x y _ _ color =
      if color <> (u8 0) then pixels := (x, y) :: !pixels
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

  let next_cycle () =
    (* Process *)
    (* TODO : a lot of reading https://wiki.nesdev.com/w/index.php/PPU_scrolling *)
    if !scanline >= 0 && !scanline < 240 then ((* 0 - 239 *)
      let cycle' = !cycle - 1 in
      let ypos = oam.(0) in
      let xpos = oam.(3) in
      if (u8 !scanline) >= ypos && (u8 cycle') >= xpos then
        sprite_0_hit := true;
      if !show_background && !cycle > 0 && !cycle <= 256 then
        (* Background *)
        let color = render_background_pixel
            Uint16.((u16 cycle') + (u16of8 !horizontal_scroll))
            Uint16.((u16 !scanline) + (u16of8 !vertical_scroll)) in
        ignore sprite_zero_check ;
        Option.may (Display.set_pixel cycle' !scanline) color
    );
    if !scanline = 241 && !cycle = 1 then (
      if not !vbl_read then vblank_enabled := true;
      if !nmi_enabled && not !vbl_read then
        Option.get !interrupt_cpu ()
    );
    (* Next *)
    incr cycle;
    if !cycle = 341 then (
      cycle := 0;
      incr scanline
    );
    if !scanline = 262 then (
      scanline := 0;
      sprite_0_hit := false;
      incr frame;
      render_sprites true 0;
      Display.display ();
      Display.clear_screen memory.(0x3F00);
      render_sprites false 0;
      if (!frame mod 2) = 1 && !show_background then cycle := 1;
      vblank_enabled := false
    );
    vbl_read := false
end

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
  Display.exit ()
