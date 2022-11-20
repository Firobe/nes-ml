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
  screen : (int32, int32_elt, c_layout) Array1.t;
  vsync : bool;
  record : (Fpath.t * string) option;
      (* name of directory where to save frames, and
         output file *)
  mutable frame : int;
}

let get_window t = t.window

let sdl_get = function
  | Error (`Msg m) -> failwith m
  | Error _ -> failwith "Unknown SDL error encountered"
  | Ok obj -> obj

let create ~width ~height ~scale ~palette ?(vsync = true) ?save title =
  let screen = Array1.create Int32 c_layout (width * height) in
  let s_width = scale * width in
  let s_height = scale * height in
  let window =
    sdl_get @@ Sdl.create_window ~w:s_width ~h:s_height title Sdl.Window.opengl
  in
  let flags = Sdl.Renderer.accelerated in
  let renderer = sdl_get @@ Sdl.create_renderer ~flags window in
  let () =
    sdl_get @@ Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend
  in
  let texture =
    sdl_get
    @@ Sdl.create_texture renderer Sdl.Pixel.format_rgb888
         Sdl.Texture.access_streaming ~w:width ~h:height
  in
  let palette = Array.of_list (List.map Int32.of_int palette) in
  let record =
    match save with
    | None -> None
    | Some out_path -> (
        match Bos.OS.Dir.tmp "nes-ml-recording%s" with
        | Ok path ->
            Printf.printf "Recording frames in %s\n%!" (Fpath.to_string path);
            Some (path, out_path)
        | Error _ ->
            Printf.printf "Couldn't create a temp dir\n%!";
            None)
  in
  {
    renderer;
    window;
    texture;
    width;
    height;
    scale;
    screen;
    palette;
    vsync;
    frame = 0;
    record;
  }

let save_mp4 dir out =
  let in_pat = Fpath.(dir / "%d.bmp" |> to_string) in
  let cmd =
    Bos.Cmd.(
      v "ffmpeg" % "-loglevel" % "error" % "-f" % "image2" % "-framerate" % "60" % "-i" % in_pat
      % "-c:v" % "libx264" % "-crf" % "0" % "-y" % "-vf"
      % "scale=1024:-1:flags=neighbor" % out)
  in
  match Bos.OS.Cmd.run cmd with
  | Ok () -> Printf.printf "Saved movie to %s\n" out
  | Error _ -> Printf.printf "Couldn't save the movie!\n"

let delete t =
  (match t.record with Some (path, out) -> save_mp4 path out | None -> ());
  Sdl.destroy_texture t.texture;
  Sdl.destroy_renderer t.renderer;
  Sdl.destroy_window t.window

let set_pixel t ~x ~y ~(color : uint8) =
  let ind = Uint8.to_int color mod 64 in
  let color = t.palette.(ind) in
  t.screen.{(y * t.width) + x} <- color

let clear t back_color =
  let rgb_color = t.palette.(Uint8.to_int back_color mod 64) in
  Array1.fill t.screen rgb_color

module FPS = struct
  let target = 60.0988 (* NTSC *)
  let cps = Sdl.get_performance_frequency ()
  let delta = 1. /. target *. Int64.to_float cps |> Int64.of_float
  let next_time = ref Int64.zero

  let wait_next_frame () =
    let now = Sdl.get_performance_counter () in
    if !next_time = Int64.zero then next_time := now;
    if now < !next_time then
      let to_wait =
        Int64.((!next_time - now) * 1000L / cps) |> Int32.of_int64
      in
      Sdl.delay to_wait
    else next_time := now;
    next_time := Int64.(!next_time + delta)
end

let save_texture pixels filename =
  let surface =
    sdl_get
    @@ Sdl.create_rgb_surface_from ~w:256 ~h:240 ~depth:32 ~pitch:256 pixels 0l
         0l 0l 0l
  in
  let filename = Fpath.to_string filename in
  sdl_get @@ Sdl.save_bmp surface filename

let render ?(after = fun () -> ()) t =
  let pixels, _ = sdl_get @@ Sdl.lock_texture t.texture None Int32 in
  Array1.blit t.screen pixels;
  Sdl.unlock_texture t.texture;
  sdl_get @@ Sdl.render_copy t.renderer t.texture;
  after ();
  (match t.record with
  | None -> ()
  | Some (path, _) ->
      let frame_name = Printf.sprintf "%d.bmp" t.frame in
      let frame_path = Fpath.(path / frame_name) in
      save_texture pixels frame_path);
  if t.vsync then FPS.wait_next_frame ();
  Sdl.render_present t.renderer;
  t.frame <- t.frame + 1

let init () = sdl_get @@ Sdl.init Sdl.Init.video
let exit () = Sdl.quit ()
(*     Option.may Sdl.free_palette !sdlpal; *)
