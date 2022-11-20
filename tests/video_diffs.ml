open Bos

let dir name = Fpath.(v name |> to_dir_path)
let roms_dir = dir "roms"
let inputs_dir = dir "inputs"
let videos_dir = dir "videos"

let launch_emulator args rom =
  let cmd = Cmd.(v "nes-ml" %% args % p rom) in
  assert (OS.Cmd.run cmd |> Result.is_ok)

let record_video name =
  let input_file = Fpath.(inputs_dir / name |> add_ext "rec") in
  let output_file = Fpath.(v name |> add_ext "mp4") in
  let rom_file = Fpath.(roms_dir / name |> add_ext "nes") in
  let args = Cmd.(v "-m" % p input_file % "-u" % "-s" % p output_file) in
  launch_emulator args rom_file;
  output_file

let videos_equal name v1 v2 =
  let get_md5 n =
    let cmd =
      Cmd.(
        v "ffmpeg" % "-loglevel" % "error" % "-i" % p n % "-map" % "0:v" % "-f"
        % "md5" % "-")
    in
    let out = OS.Cmd.run_out cmd in
    match OS.Cmd.to_string out with
    | Ok s -> s
    | Error _ -> failwith "Couldn't retrieve output"
  in
  let m1 = get_md5 v1 in
  let m2 = get_md5 v2 in
  Printf.printf "%s\n=============\nv1: %s\nv2: %s\n" name m1 m2;
  m1 = m2

let test_case name =
  let test_video = record_video name in
  let video_file = Fpath.(videos_dir / name |> add_ext "mp4") in
  assert (videos_equal name test_video video_file)

let list_inputs () =
  match OS.Dir.contents inputs_dir with
  | Ok files -> List.map Fpath.rem_ext files |> List.map Fpath.basename
  | Error _ -> failwith "Failed to retrieve list of recordings"

let _ =
  let names = list_inputs () in
  List.iter test_case names
