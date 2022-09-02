open Tsdl
open Bigarray
open Stdint

type t = {
  renderer : Sdl.renderer;
  window : Sdl.window;
  texture : Sdl.texture;
  width : int;
  height : int;
  scale : int;
  palette : int32 array;
  screen : (int32, int32_elt, c_layout) Array1.t
}

let get_window t = t.window

let sdl_get = function
  | Error (`Msg m) -> failwith m
  | Error _ -> failwith "Unknown SDL error encountered"
  | Ok obj -> obj

let create ~width ~height ~scale ~palette title =
  let screen = Array1.create Int32 c_layout (width * height) in
  let s_width = scale * width in
  let s_height = scale * height in
  let window = sdl_get @@ Sdl.create_window ~w:s_width ~h:s_height title Sdl.Window.opengl in
  let flags = Sdl.Renderer.accelerated in
  let renderer = sdl_get @@ Sdl.create_renderer ~flags window in
  let () = sdl_get @@ Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend in
  let texture = sdl_get @@ Sdl.create_texture renderer Sdl.Pixel.format_rgb888
      Sdl.Texture.access_streaming ~w:width ~h:height in
  let palette = Array.of_list (List.map Int32.of_int palette) in
  {renderer; window; texture; width; height; scale; screen; palette}

let delete t =
  Sdl.destroy_texture t.texture;
  Sdl.destroy_renderer t.renderer;
  Sdl.destroy_window t.window

let set_pixel t ~x ~y ~(color : uint8) =
  let ind = (Uint8.to_int color) mod 64 in
  let color = t.palette.(ind) in
  t.screen.{y * t.width + x} <- color

let clear t back_color =
  let rgb_color = t.palette.(Uint8.to_int back_color mod 64) in
  Array1.fill t.screen rgb_color

module FPS = struct
  let target = 60.0988 (* NTSC *)
  let cps = Sdl.get_performance_frequency ()
  let delta = ((1. /. target) *. (Int64.to_float cps)) |> Int64.of_float
  let next_time = ref (Int64.zero)
  let wait_next_frame () =
    let now = Sdl.get_performance_counter () in
    if !next_time = Int64.zero then (next_time := now);
    if now < !next_time then (
      let to_wait = Int64.((!next_time - now) * 1000L / cps) |> Int32.of_int64 in
      Sdl.delay to_wait
    ) else (next_time := now);
    next_time := Int64.(!next_time + delta)
end

let render ?(after = fun () -> ()) t =
  let pixels, _ = sdl_get @@ Sdl.lock_texture t.texture None Int32 in
  Array1.blit t.screen pixels;
  Sdl.unlock_texture t.texture;
  sdl_get @@ Sdl.render_copy t.renderer t.texture;
  after ();
  FPS.wait_next_frame ();
  Sdl.render_present t.renderer

let init () = sdl_get @@ Sdl.init Sdl.Init.video
let exit () = Sdl.quit ()
(*     Option.may Sdl.free_palette !sdlpal; *)
