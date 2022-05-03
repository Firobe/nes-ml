open Tsdl

(* TODO: separate backend type from the main state *)

(* in Hz *)
let master_freq = 21477272
let cpu_freq = 1789773 
let main_divider = 89490 (* to obtain frame counter *)
let cpu_divider = main_divider / (master_freq / cpu_freq)

module Divider = struct
  type t = {
    mutable length : int;
    mutable counter : int
  }

  let create length = {length; counter = length}
  let reload t = t.counter <- t.length
  let set_length t length = t.length <- length

  (* Returns if the divider clocks *)
  let clock t =
    if t.counter = 0 then (
      reload t; true
    ) else (
      t.counter <- t.counter - 1; false
    )
end
(*
module Float_divider = struct
  let precision = 4
  type t = {
    main_divider : Divider.t;

  }
  let get_periods f_period =
    let main_period = int_of_float f_period in
    let sub_period = 1 in

  let create f_period = {
    let m, s = get_periods f_period in
    main_divider = Divider.create m;
    sub_divider = Divider.create s
  }

  let clock t =
    if Divider.clock 
end
   *)

module Sequencer = struct
  type t = {
    mutable length : int;
    mutable step : int
  }
  let create length = {length; step = 0;}
  let set_length t length = t.length <- length
  let clock t = t.step <- (t.step + 1) mod t.length
  let reset t = t.step <- 0
  let get t = t.step
end

module Counter = struct
  type t = {mutable counter : int; mutable halt : bool}
  let active t = t.counter > 0
  let load t v = t.counter <- v
  let update t halt = t.halt <- halt
  let reset t = t.counter <- 0
  let clock t =
    if not t.halt && t.counter > 0 then
      t.counter <- t.counter - 1
  let create () = {counter = 0; halt = true}
end

module Length_counter = struct
  include Counter
  let lengths = [|
    10;254; 20;  2; 40;  4; 80;  6; 160;  8; 60; 10; 14; 12; 26; 14;
    12; 16; 24; 18; 48; 20; 96; 22; 192; 24; 72; 26; 16; 28; 32; 30
  |]
  let load t v = load t lengths.(v)
end

module Pulse = struct
  (* TODO sweep *)

  let duties = [|
    [|0; 1; 0; 0; 0; 0; 0; 0|];
    [|0; 1; 1; 0; 0; 0; 0; 0|];
    [|0; 1; 1; 1; 1; 0; 0; 0|];
    [|1; 0; 0; 1; 1; 1; 1; 1|]
  |]

  type t = {
    timer : Divider.t;
    sequencer : Sequencer.t;
    length : Length_counter.t;
    mutable duty_type : int;
    mutable enabled : bool;
  }

  let create () = {
    timer = Divider.create 0;
    sequencer = Sequencer.create 8;
    length = Length_counter.create ();
    duty_type = 0;
    enabled = false;
  }

  let active t = Length_counter.active t.length

  let write0 t v =
    t.duty_type <- (v lsr 6);
    Length_counter.update t.length ((v land 0x8) <> 0)

  let update t v =
    t.enabled <- v;
    if not t.enabled then Length_counter.reset t.length 

  let write1 _ _ = ()

  let write2 t v =
    let new_length = (t.timer.length land 0x700) lor v in
    Divider.set_length t.timer new_length

  let write3 t v =
    let new_length = (t.timer.length land 0xFF) lor ((v land 0x7) lsl 8) in
    Divider.set_length t.timer new_length;
    Length_counter.load t.length (v lsr 3);
    Sequencer.reset t.sequencer

  let clock t =
    if Divider.clock t.timer then (
      Sequencer.clock t.sequencer
    )

  let frame_clock t =
    if t.enabled then Length_counter.clock t.length

  let output t = 
    if t.timer.length >= 8 && Length_counter.active t.length then (
      (* this should be the envelope instead of 15 TODO *)
      15 * duties.(t.duty_type).(Sequencer.get t.sequencer)
    )
    else 0
end

module Triangle = struct
  let sequence = [|
    15; 14; 13; 12; 11; 10;  9;  8;  7;  6;  5;  4;  3;  2;  1;  0;
    0;  1;  2;  3;  4;  5;  6;  7;  8;  9; 10; 11; 12; 13; 14; 15
  |]

  type t = {
    mutable enabled : bool;
    timer : Divider.t;
    sequencer : Sequencer.t;
    length : Length_counter.t;
    linear_counter : Counter.t;
    mutable linear_reload : int;
    mutable control : bool
  }

  let create () = {
    enabled = false;
    timer = Divider.create 0;
    sequencer = Sequencer.create 32;
    linear_counter = Counter.create ();
    length = Length_counter.create ();
    linear_reload = 0;
    control = false
  }

  let active t = Length_counter.active t.length

  let update t v =
    t.enabled <- v;
    if not t.enabled then Length_counter.reset t.length 

  let write_linear t v =
    let value = v land 0x7F in
    let control = (v land 0x80) <> 0 in
    t.linear_reload <- value;
    Counter.load t.linear_counter value;
    Length_counter.update t.length control;
    t.control <- control

  let write_a t v =
    let new_length = (t.timer.length land 0x700) lor v in
    Divider.set_length t.timer new_length

  let write_b t v =
    let new_length = (t.timer.length land 0xFF) lor ((v land 0x7) lsl 8) in
    Divider.set_length t.timer new_length;
    Length_counter.load t.length (v lsr 3);
    Sequencer.reset t.sequencer;
    t.linear_counter.halt <- false

  let clock t =
    if Divider.clock t.timer && Counter.active t.linear_counter
       && Length_counter.active t.length then (
      Sequencer.clock t.sequencer
    )

  let linear_frame_clock t =
    if t.linear_counter.halt then
      t.linear_counter.counter <- t.linear_reload
    else (Counter.clock t.linear_counter);
    if not t.control then Counter.update t.linear_counter false

  let frame_clock t =
    if t.enabled then (Length_counter.clock t.length)

  let output t = sequence.(Sequencer.get t.sequencer)
end

module Frame_counter = struct
  module Event = struct
    type t = O | E | EL | ELF
    let is_e t = (t <> O)
    let is_l = function EL | ELF -> true | _ -> false
    let is_f t = (t = ELF)
  end
  let mode1 = Event.[|E; EL; E; ELF|]
  let mode2 = Event.[|EL; E; EL; E; O|]

  type t = {
    divider : Divider.t;
    sequencer : Sequencer.t;
    mutable mode : bool;
    mutable frame_interrupt : bool
  }

  let create () = {
    divider = Divider.create cpu_divider;
    sequencer = Sequencer.create 4;
    mode = false;
    frame_interrupt = false
  }

  let write t v =
    Divider.reload t.divider;
    Sequencer.reset t.sequencer;
    let mode = (v lsr 7) <> 0 in
    t.mode <- mode;
    Sequencer.set_length t.sequencer
      (if mode then 5 else 4);
    Sequencer.clock t.sequencer

  let action t (pulse1, pulse2, triangle) =
    let mode_array = if t.mode then mode2 else mode1 in
    let event = mode_array.(Sequencer.get t.sequencer) in
    if Event.is_e event then (
      Triangle.linear_frame_clock triangle;
      (* TODO clock envelope *)
    );
    if Event.is_l event then (
      (* clock length counters and sweep units TODO *)
      Pulse.frame_clock pulse1;
      Pulse.frame_clock pulse2;
      Triangle.frame_clock triangle
    );
    t.frame_interrupt <- Event.is_f event
    (* TODO actually interrupt the CPU *)

  let clock t units = 
    if Divider.clock t.divider then (
      Sequencer.clock t.sequencer;
      action t units
    )
end

module APU = struct
  type audio_backend = {
    device : int32;
    sampling_freq : int;
  }

  type t =  {
    frame_counter : Frame_counter.t;
    pulse1 : Pulse.t;
    pulse2 : Pulse.t;
    triangle : Triangle.t;
    resampler : Divider.t;
    half_clock : Divider.t;
    backend : audio_backend;
  }

  (* to be adjusted dynamically *)
  let sampling_period b = cpu_freq / b.sampling_freq

  let create backend = {
    frame_counter = Frame_counter.create ();
    pulse1 = Pulse.create ();
    pulse2 = Pulse.create ();
    triangle = Triangle.create ();
    resampler = Divider.create (sampling_period backend - 1);
    half_clock = Divider.create 1;
    backend
  }

  let write_register t v r =
    let open Stdint in
    let v = Uint8.to_int v in
    let r = Uint16.to_int r in
    match r with
    | 0x4000 -> Pulse.write0 t.pulse1 v
    | 0x4001 -> Pulse.write1 t.pulse1 v
    | 0x4002 -> Pulse.write2 t.pulse1 v
    | 0x4003 -> Pulse.write3 t.pulse1 v
    | 0x4004 -> Pulse.write0 t.pulse2 v
    | 0x4005 -> Pulse.write1 t.pulse2 v
    | 0x4006 -> Pulse.write2 t.pulse2 v
    | 0x4007 -> Pulse.write3 t.pulse2 v
    | 0x4008 -> Triangle.write_linear t.triangle v
    | 0x400A -> Triangle.write_a t.triangle v
    | 0x400B -> Triangle.write_b t.triangle v
    | 0x4015 -> (* status *)
      let e_pulse1 = (v land 0x1) <> 0 in
      let e_pulse2 = (v land 0x2) <> 0 in
      let e_triangle = (v land 0x4) <> 0 in
      let _e_noise = (v land 0x8) <> 0 in
      let _e_dmc = (v land 0x10) <> 0 in
      Pulse.update t.pulse1 e_pulse1;
      Pulse.update t.pulse2 e_pulse2;
      Triangle.update t.triangle e_triangle
      (* TODO update other stuff *)
    | 0x4017 -> Frame_counter.write t.frame_counter v
    | _ -> ()

  let read_register t r =
    if (Stdint.Uint16.to_int r) = 0x4015 then (
      let iob n b = if b then 1 lsl n else 0 in
      let a_pulse1 = Pulse.active t.pulse1 |> iob 0 in
      let a_pulse2 = Pulse.active t.pulse1 |> iob 1 in
      let a_triangle = Triangle.active t.triangle |> iob 2 in
      let interrupt = t.frame_counter.frame_interrupt |> iob 6 in
      a_pulse1 lor a_pulse2 lor interrupt lor a_triangle |> Stdint.Uint8.of_int
      (* TODO other bits *)
    ) else failwith "Read invalid APU register"

  let mixer t =
    let pulse1 = (float_of_int @@ Pulse.output t.pulse1) in
    let pulse2 = (float_of_int @@ Pulse.output t.pulse2) in
    let triangle = (float_of_int @@ Triangle.output t.triangle) in
    let noise = 0. in
    let dmc = 0. in
    let pulse_out = 95.88 /. (8128. /. (pulse1 +. pulse2) +. 100.) in
    let tnd_factor = triangle /. 8227. +. noise /. 12241. +. dmc /. 22638. in
    let tnd_out = 159.79 /. (1. /. tnd_factor +. 100.) in
    pulse_out +. tnd_out

  let push_sample =
    let mini_buffer =
      Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout 1 in
    fun t -> (
        let value = mixer t in (* [0. - 1. ] *)
        mini_buffer.{0} <- value;
        match Sdl.queue_audio t.backend.device mini_buffer with
        | Ok () -> ()
        | Error (`Msg e) ->
          Printf.printf "Error when pushing samples: %s\n" e;
          assert false
      )

  let next_cycle t =
    (* Clock pulse timers *)
    if Divider.clock t.half_clock then (
      Frame_counter.clock t.frame_counter (t.pulse1, t.pulse2, t.triangle);
      Pulse.clock t.pulse1;
      Pulse.clock t.pulse2;
    );
    Triangle.clock t.triangle;
    if Divider.clock t.resampler then push_sample t

  let exit t = Sdl.close_audio_device t.backend.device
end

include APU

let create () =
  match Sdl.init Sdl.Init.audio with
  | Error (`Msg e) ->
    Printf.printf "Error while initializing audio device %s" e;
    assert false
  | Ok () -> () ;
    let audio_spec = {
      Sdl.as_freq = 44100;
      as_format = Sdl.Audio.f32;
      as_channels = 1;
      as_silence = 0;
      as_samples = 1024;
      as_size = Int32.zero;
      as_callback = None
    } in
    let (dev, have) = match Sdl.open_audio_device None false audio_spec
                              Sdl.Audio.allow_frequency_change with
    | Error (`Msg e) ->
      Printf.printf "Error while opening audio device: %s\n" e;
      assert false
    | Ok h -> h
    in
    Sdl.pause_audio_device dev false;
    let backend = APU.{
      sampling_freq = have.as_freq;
      device = dev
    } in
    APU.create backend
