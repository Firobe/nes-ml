open Tsdl

let cycle = ref 0
let device = ref None
let sampling = ref 0.
let volume = 10000.
let callback_ref = ref None

let duties = [|
  [|0; 1; 0; 0; 0; 0; 0; 0|];
  [|0; 1; 1; 0; 0; 0; 0; 0|];
  [|0; 1; 1; 1; 1; 0; 0; 0|];
  [|1; 0; 0; 0; 1; 1; 1; 1|]
|]

let timer = ref 0
let timer_step = ref 0
let duty_type = ref 0
let duty_step = ref 0

let write_register v r =
  let open Stdint in
  let v = Uint8.to_int v in
  let r = Uint16.to_int r in
  match r with
  | 4000 ->
    duty_type := (v lsr 6)
  | 4001 -> ()
  | 4002 ->
    timer := (!timer land 0x700) lor v;
    duty_step := 0;
    timer_step := 0
  | 4003 -> 
    timer := (!timer land 0xFF) lor (v lsl 8);
    duty_step := 0;
    timer_step := 0
  | _ -> ()

let next_cycle () =
  incr cycle;
  if !timer_step = 0 then (
    (* Clock *)
    duty_step := (!duty_step + 1) mod 8;
    timer_step := !timer
  )

let mixer () =
  0.00752 *. (float_of_int duties.(!duty_type).(!duty_step))

let callback buffer =
  let amplified = (mixer ()) *. volume in
  let toWrite = int_of_float amplified in
  Printf.printf "%d %d\n%!" (Bigarray.Array1.dim buffer) toWrite;
  for i = 0 to Bigarray.Array1.dim buffer - 1 do
    buffer.{i} <- toWrite
  done

let init () =
  match Sdl.init Sdl.Init.audio with
  | Error (`Msg e) ->
    Printf.printf "Error while initializing audio device %s" e;
    assert false
  | Ok () -> () ;
    callback_ref :=
      Some (Sdl.audio_callback Bigarray.Int8_unsigned callback) ;
    let audio_spec = {
      Sdl.as_freq = 44100;
      as_format = Sdl.Audio.u8;
      as_channels = 1;
      as_silence = 0;
      as_samples = 1024;
      as_size = Int32.zero;
      as_callback = !callback_ref
    } in
    let (dev, have) = match Sdl.open_audio_device None false audio_spec
                              Sdl.Audio.allow_frequency_change with
    | Error (`Msg e) ->
      Printf.printf "Error while opening audio device: %s\n" e;
      assert false
    | Ok h -> h
    in
    sampling := float_of_int have.as_freq ;
    device := Some dev ;
    Sdl.pause_audio_device dev false

let exit () =
  Option.may Sdl.close_audio_device !device
