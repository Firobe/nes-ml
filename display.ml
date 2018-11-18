open Tsdl

let scale = 3
let width = scale * 256
let height = scale * 240

let palette_l = [84;84;84;0;30;116;8;16;144;48;0;136;68;0;100;92;0;48;84;4;0;60;24;0;32;42;0;8;58;0;0;64;0;0;60;0;0;50;60;0;0;0;0;0;0;0;0;0;152;150;152;8;76;196;48;50;236;92;30;228;136;20;176;160;20;100;152;34;32;120;60;0;84;90;0;40;114;0;8;124;0;0;118;40;0;102;120;0;0;0;0;0;0;0;0;0;236;238;236;76;154;236;120;124;236;176;98;236;228;84;236;236;88;180;236;106;100;212;136;32;160;170;0;116;196;0;76;208;32;56;204;108;56;180;204;60;60;60;0;0;0;0;0;0;236;238;236;168;204;236;188;188;236;212;178;236;236;174;236;236;174;212;236;180;176;228;196;144;204;210;120;180;222;120;168;226;144;152;226;180;160;214;228;160;162;160;0;0;0;0;0;0]

let palette = Array.of_list palette_l

let get_color c =
    let n = c land 0x3F in
    let r = palette.(n * 3) in
    let g = palette.(n * 3 + 1) in
    let b = palette.(n * 3 + 2) in
    (r, g, b)

let renderer = ref None
let window = ref None

let clear_screen () =
    let r = Option.get !renderer in
    ignore (Sdl.set_render_draw_color r 0 0 0 255) ;
    ignore (Sdl.render_clear r)

let display screen =
    let rend = Option.get !renderer in
    clear_screen ();
    for y = 0 to 239 do
        for x = 0 to 255 do
            let r, g, b = get_color screen.(x).(y) in
            (* 240 - y *)
            let rect = Sdl.Rect.create ~x:(x * scale) ~y:(y * scale) ~w:scale ~h:scale in
            ignore (Sdl.set_render_draw_color rend r g b 255) ;
            ignore (Sdl.render_fill_rect rend (Some rect))
        done
    done;
    ignore (Sdl.render_present rend)

let sdl_get = function
    | Error _ -> failwith "SDL error encountered"
    | Ok obj -> obj

let init () =
    ignore (Sdl.init Sdl.Init.video) ;
    let window = sdl_get @@ Sdl.create_window ~w:width ~h:height "NES" Sdl.Window.opengl in
    let flags = Sdl.Renderer.(+) Sdl.Renderer.accelerated Sdl.Renderer.presentvsync in
    let r = sdl_get @@ Sdl.create_renderer ~flags window in
    renderer := Some r ;
    clear_screen ()

let exit () =
    Option.may Sdl.destroy_renderer !renderer;
    Option.may Sdl.destroy_window !window;
    Sdl.quit ()
