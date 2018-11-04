let memory = Array.make 0x4000 0x0
let oam = Array.make 0x100 0x0

(* Status *)
let vblank_enabled = ref true

(* Scrolling *)
let horizontal_scroll = ref 0
let vertical_scroll = ref 0

(* Control register *)
let base_nametable_address = ref 0x2000
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

let oam_address = ref 0x0
let ppu_address = ref 0x0

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
(*     Printf.printf "Register %d <- 0x%X\n" register v; *)
    match register with
    | 0 -> (* Control register *)
        base_nametable_address := v land 0x3;
        ppudata_increment := if nth_bit v 2 then 32 else 1;
        sprite_pattern_address := if (nth_bit v 3) then 0x0 else 0x1000;
        background_pattern_address := if (nth_bit v 4) then 0x0 else 0x1000;
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
    | 2 -> assert false
    | 3 -> (* OAM address *)
        oam_address := v
    | 4 -> (* OAM data *)
        Array.set oam !oam_address v;
        oam_address := !oam_address + 1
    | 5 -> (* Scroll register *)
        if read_latch () then
            horizontal_scroll := v
        else
            vertical_scroll := v
    | 6 -> (* PPU address *)
        if read_latch () then
            ppu_address := (!ppu_address land 0xFF) lor (v lsl 8)
        else
            ppu_address := (!ppu_address land 0xFF00) lor v
    | 7 -> (* PPU data *)
        Array.set memory !ppu_address v;
        ppu_address := !ppu_address + !ppudata_increment
    | _ -> assert false

let get_register addr =
    let register = addr land 0x7 in
(*     Printf.printf "Register %d -> read\n" register; *)
    match register with
    | 2 -> (* Status register *)
        latch := true;
        (int_of_bool !vblank_enabled) lsl 7
    | 4 -> (* OAM data *)
        let r = Array.get oam !oam_address in
        oam_address := !oam_address + 1; r
    | 7 -> (* PPU data *)
        let r = Array.get memory !ppu_address in
        ppu_address := !ppu_address + !ppudata_increment; r
    | _ -> assert false

let dump_memory () =
    let file = open_out_bin "memdump_vram" in
    let store = Bytes.create 0x10000 in
    for i = 0 to (Array.length memory) - 1 do
        Bytes.set store i @@ char_of_int memory.(i)
    done ;
    output file store 0 (Bytes.length store) ;
    close_out file

let render () = ()
