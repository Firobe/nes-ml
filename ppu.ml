let memory = Array.make 0x4000 0x0
let oam = Array.make 0x100 0x0

(* Status *)
let vblank_enabled = ref true

(* Scrolling *)
let horizontal_scroll = ref 0
let vertical_scroll = ref 0

(* Control register *)
let base_nametable = ref 0x2000
let ppudata_increment = ref 0x1
let sprite_pattern_address = ref 0x0
let background_pattern_address = ref 0x0
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

let oam_address = ref 0x0
let ppu_address = ref 0x0

let interrupt_cpu = ref None

(* Latch for PPUSCROLL and PPUADDR *)
let latch = ref true (* True : first set *)
let read_latch () =
    let r = !latch in
    latch := not !latch;
    r

let int_of_bool b = if b then 1 else 0
let nth_bit b n =
    (b land (1 lsl n)) != 0

let set_register addr v =
    let register = addr land 0x7 in
    match register with
    | 0 -> (* Control register *)
        base_nametable := v land 0x3;
        ppudata_increment := if nth_bit v 2 then 32 else 1;
        sprite_pattern_address := if (nth_bit v 3) then 0x1000 else 0x0;
        background_pattern_address := if (nth_bit v 4) then 0x1000 else 0x0;
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
        Array.set oam !oam_address v;
        oam_address := (!oam_address + 1) mod 0x100
    | 5 -> (* Scroll register *)
        if read_latch () then
            horizontal_scroll := v
        else
            vertical_scroll := v
    | 6 -> (* PPU address *)
        if read_latch () then
            ppu_address := ((!ppu_address land 0xFF) lor (v lsl 8) land 0x3FFF)
        else
            ppu_address := (!ppu_address land 0xFF00) lor v
    | 7 -> (* PPU data *)
        Array.set memory !ppu_address v;
        ppu_address := (!ppu_address + !ppudata_increment) land 0x3FFF
    | _ -> Printf.printf "Warning: trying to set 0x800%d\n" register

let vram_buffer = ref 0
let get_register addr =
    let register = addr land 0x7 in
    match register with
    | 2 -> (* Status register *)
        latch := true;
        let r = (int_of_bool !vblank_enabled) lsl 7 in
        vblank_enabled := false; r
    | 4 -> (* OAM data *)
        Array.get oam !oam_address
    | 7 -> (* PPU data *)
        let oldb = !vram_buffer in
        vram_buffer := Array.get memory !ppu_address;
        let r = if !ppu_address < 0x3F00 then oldb else !vram_buffer in
        ppu_address := !ppu_address + !ppudata_increment;
        r
    | _ -> Printf.printf "Warning: trying to read 0x800%d\n" register; 0

let dma mem cpu_begin =
    let rec aux cpu_addr oam_addr length =
        if length > 0 then (
            oam.(oam_addr) <- mem.(cpu_addr);
            aux (cpu_addr + 1) ((oam_addr + 1) mod 0x100) (length - 1)
        )
    in aux cpu_begin !oam_address 0x100

let dump_memory () =
    let file = open_out_bin "memdump_vram" in
    let store = Bytes.create 0x10000 in
    for i = 0 to (Array.length memory) - 1 do
        Bytes.set store i @@ char_of_int memory.(i)
    done ;
    output file store 0 (Bytes.length store) ;
    close_out file

let decode_chr start tile_nb x y =
    let chr_base = start + tile_nb * 0x10 in
    let x_mod = x mod 8 in
    let y_mod = y mod 8 in
    let low_byte = memory.(chr_base + y_mod) in
    let high_byte = memory.(chr_base + 0x8 + y_mod) in
    let mask = 1 lsl (7 - x_mod) in
    let low_bit = int_of_bool (low_byte land mask != 0) in
    let high_bit = int_of_bool (high_byte land mask != 0) in
    low_bit lor (high_bit lsl 1)

let get_address x y =
    let x_add = x + 32 * (!base_nametable land 1) in
    let y_add = y + 30 * (!base_nametable lsr 1) in 
    let x_mir = x_add mod (if !mirroring_mode then 64 else 32) in
    let y_mir = y_add mod (if !mirroring_mode then 30 else 60) in
    let quad_nb = (y_mir / 30) * 2 + (x_mir / 32) in
    let base = 0x2000 + 0x400 * quad_nb in
    base, (x mod 32), (y mod 30)

let render_background_pixel x y =
    let x_tile = x / 8 in
    let y_tile = y / 8 in
    let base_addr, x_mod, y_mod = get_address x_tile y_tile in
    let address = base_addr + y_mod * 32 + x_mod in
    let tile_kind = memory.(address) in
    let color_nb = decode_chr !background_pattern_address tile_kind x y in
    match color_nb with
    | 0 -> None
    | _ ->
        (* Decode attribute table *)
        let attr_table_address = base_addr + 0x3C0 in
        let x_big = x_mod / 4 in
        let y_big = y_mod / 4 in
        let big_addr = attr_table_address + y_big * 8 + x_big in
        let big_byte = memory.(big_addr) in
        let block_offset = (((x_mod / 2) mod 2) + 2 * ((y_mod / 2) mod 2)) * 2 in
        let palette_nb = (big_byte lsr block_offset) land 0x3 in
        (* Get palette *)
        let address = 0x3F00 + palette_nb * 4 + color_nb in
        Some memory.(address)

let render_background () =
    for y = 0 to 239 do
        for x = 0 to 255 do
            let color = render_background_pixel
                (x + !horizontal_scroll) (y + !vertical_scroll) in
            Option.may (Display.set_pixel x y) color
        done
    done

let render_sprite nb =
    if !sprite_size then Printf.printf "Unsupported 8x16 sprites\n";
    let ypos = oam.(nb) in
    let xpos = oam.(nb + 3) in
    let attributes = oam.(nb + 2) in
    let tile_nb = oam.(nb + 1) in
    let palette = attributes land 0x3 in
    let flip_h = nth_bit attributes 6 in
    let flip_v = nth_bit attributes 7 in
    let palette_addr = 0x3F10 + palette * 4 in
    for y = 0 to 7 do
        if ypos + y < 240 then
            for x = 0 to 7 do
                if xpos + x < 256 then
                    let fx = if flip_h then 7 - x else x in
                    let fy = if flip_v then 7 - y else y in
                    let color_nb = decode_chr !sprite_pattern_address
                        tile_nb fx fy in
                    if color_nb != 0  then
                        let color =  memory.(palette_addr + color_nb) in
                        Display.set_pixel (x + xpos) (y + ypos) color
            done
    done

let rec render_sprites after_back nb =
    if nb != 256 then (
        if ((oam.(nb + 2) land 0x20 != 0) != after_back) then
            render_sprite nb
        ;
        render_sprites after_back (nb + 4)
   )

let render () =
    vblank_enabled := false;
    Display.clear_screen memory.(0x3F00);
    if !show_sprites then
        render_sprites false 0;
    if !show_background then
        render_background ();
    if !show_sprites then
        render_sprites true 0;
    Display.display ();
    vblank_enabled := true;
    if !nmi_enabled then
        Option.get !interrupt_cpu ()

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
    
let init ic mm =
    interrupt_cpu := Some ic;
    mirroring_mode := mm;
    Display.init ()

let exit () =
    Display.exit ()
