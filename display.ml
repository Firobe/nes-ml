open Tsdl
open Stdint

let width = 256
let height = 240
let scale = 4

let s_width = scale * width
let s_height = scale * height

let palette = Array.of_list @@ List.map Int32.of_int
[0x7C7C7C; 0x0000FC; 0x0000BC; 0x4428BC; 0x940084; 0xA80020; 0xA81000; 0x881400;
 0x503000; 0x007800; 0x006800; 0x005800; 0x004058; 0x000000; 0x000000; 0x000000;
 0xBCBCBC; 0x0078F8; 0x0058F8; 0x6844FC; 0xD800CC; 0xE40058; 0xF83800; 0xE45C10;
 0xAC7C00; 0x00B800; 0x00A800; 0x00A844; 0x008888; 0x000000; 0x000000; 0x000000;
 0xF8F8F8; 0x3CBCFC; 0x6888FC; 0x9878F8; 0xF878F8; 0xF85898; 0xF87858; 0xFCA044;
 0xF8B800; 0xB8F818; 0x58D854; 0x58F898; 0x00E8D8; 0x787878; 0x000000; 0x000000;
 0xFCFCFC; 0xA4E4FC; 0xB8B8F8; 0xD8B8F8; 0xF8B8F8; 0xF8A4C0; 0xF0D0B0; 0xFCE0A8;
 0xF8D878; 0xD8F878; 0xB8F8B8; 0xB8F8D8; 0x00FCFC; 0xF8D8F8; 0x000000; 0x000000]

let renderer = Global.empty "renderer"
let window = Global.empty "window"
let texture = Global.empty "texture"

let screen = Bigarray.(Array1.create Int32 c_layout (width * height))

let set_pixel x y (v : uint8) =
    let color = palette.(Uint8.to_int v) in
    screen.{y * width + x} <- color

let sdl_get = function
    | Error (`Msg m) -> failwith m
    | Error _ -> failwith "Unknown SDL error encountered"
    | Ok obj -> obj

let clear_screen back_color =
    let rgb_color = palette.(Uint8.to_int back_color) in
    Bigarray.Array1.fill screen rgb_color

let display () =
    let rend = Global.get renderer in
    let text = Global.get texture in
    let pixels, _ = sdl_get @@ Sdl.lock_texture text
        None Int32 in
    Bigarray.Array1.blit screen pixels;
    Sdl.unlock_texture text;
    sdl_get @@ Sdl.render_copy rend text ;
    Sdl.render_present rend

let init () =
    sdl_get @@ Sdl.init Sdl.Init.video ;
    let win = sdl_get @@ Sdl.create_window ~w:s_width ~h:s_height "NES" Sdl.Window.opengl in
    let flags = Sdl.Renderer.(+) Sdl.Renderer.accelerated
    Sdl.Renderer.accelerated in
    let r = sdl_get @@ Sdl.create_renderer ~flags win in
(*     let pal = sdl_get @@ Sdl.alloc_palette (Array.length palette) in *)
    let text = sdl_get @@ Sdl.create_texture r Sdl.Pixel.format_rgb888
        Sdl.Texture.access_streaming ~w:256 ~h:240 in
    Global.set window win;
    Global.set renderer r;
    Global.set texture text

let exit () =
(*     Option.may Sdl.free_palette !sdlpal; *)
    Option.may Sdl.destroy_texture (Global.opt texture);
    Option.may Sdl.destroy_renderer (Global.opt renderer);
    Option.may Sdl.destroy_window (Global.opt window);
    Sdl.quit ()
