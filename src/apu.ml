open Infix_int.Common
open Tsdl

let dynamic_rate_control = true

(* in Hz *)
let master_freq = 21477272
let cpu_freq = 1789773
let main_divider = 89490 (* to obtain frame counter *)
let cpu_divider = main_divider / (master_freq / cpu_freq)

(* Adjust overall output volume *)
let volume_modifier = 0.1

module Divider = struct
  type t = { mutable length : int; mutable counter : int }

  let create length = { length; counter = length }
  let reload t = t.counter <- t.length
  let set_length t length = t.length <- length

  (* Returns if the divider clocks *)
  let clock t =
    if t.counter = 0 then (
      reload t;
      true)
    else (
      t.counter <- t.counter - 1;
      false)
end

module Sequencer = struct
  type t = { mutable length : int; mutable step : int }

  let create length = { length; step = 0 }
  let set_length t length = t.length <- length
  let clock t = t.step <- (t.step + 1) mod t.length
  let reset t = t.step <- 0
  let get t = t.step
end

module Counter = struct
  type t = { mutable counter : int; mutable halt : bool }

  let active t = t.counter > 0
  let load t v = t.counter <- v
  let update t halt = t.halt <- halt
  let reset t = t.counter <- 0
  let clock t = if (not t.halt) && t.counter > 0 then t.counter <- t.counter - 1
  let create () = { counter = 0; halt = true }
end

module Length_counter = struct
  include Counter

  let lengths =
    [|
      10;
      254;
      20;
      2;
      40;
      4;
      80;
      6;
      160;
      8;
      60;
      10;
      14;
      12;
      26;
      14;
      12;
      16;
      24;
      18;
      48;
      20;
      96;
      22;
      192;
      24;
      72;
      26;
      16;
      28;
      32;
      30;
    |]

  let load t v = load t lengths.(v)
end

module Envelope = struct
  type t = {
    mutable start : bool;
    mutable volume : int;
    mutable constant : bool;
    mutable decay : int;
    mutable loop : bool;
    divider : Divider.t;
  }

  let create () =
    {
      start = false;
      volume = 0;
      loop = false;
      constant = false;
      decay = 0;
      divider = Divider.create 0;
    }

  let set_constant t b = t.constant <- b
  let set_start t = t.start <- true
  let set_loop t b = t.loop <- b
  let set_volume t v = t.volume <- v

  let clock t =
    if t.start then (
      t.start <- false;
      t.decay <- 15;
      Divider.set_length t.divider t.volume)
    else if Divider.clock t.divider then
      if t.decay = 0 && t.loop then t.decay <- 15
      else if t.decay > 0 then t.decay <- t.decay - 1

  let output t = if t.constant then t.volume else t.decay
end

module Sweep = struct
  type pulse_kind = Pulse1 | Pulse2

  type t = {
    divider : Divider.t;
    kind : pulse_kind;
    mutable shift_count : int;
    mutable target_period : int;
    mutable enabled : bool;
    mutable reload : bool;
    mutable negate_flag : bool;
    mutable current_period : int;
  }

  let muted t = t.target_period > 0x7FF

  let create kind =
    {
      divider = Divider.create 0;
      target_period = 0;
      current_period = 0;
      kind;
      shift_count = 0;
      enabled = false;
      negate_flag = false;
      reload = false;
    }

  let new_target t period =
    t.current_period <- period;
    let change_amount = period lsr t.shift_count in
    let change_amount =
      if t.negate_flag then
        match t.kind with
        | Pulse1 -> -change_amount - 1
        | Pulse2 -> -change_amount
      else change_amount
    in
    let target = period + change_amount in
    t.target_period <- target

  let write t v =
    t.enabled <- v land 0x80 <> 0;
    let period = (v lsr 4) land 0x7 in
    Divider.set_length t.divider period;
    t.negate_flag <- v land 0x8 <> 0;
    t.shift_count <- v land 0x7;
    t.reload <- true

  let clock t =
    if t.enabled && (not @@ muted t) && Divider.clock t.divider then (
      t.reload <- false;
      new_target t t.target_period);
    if t.reload then (
      Divider.reload t.divider;
      t.reload <- false)
end

module Pulse = struct
  let duties =
    [|
      [| 0; 1; 0; 0; 0; 0; 0; 0 |];
      [| 0; 1; 1; 0; 0; 0; 0; 0 |];
      [| 0; 1; 1; 1; 1; 0; 0; 0 |];
      [| 1; 0; 0; 1; 1; 1; 1; 1 |];
    |]

  type t = {
    timer : Divider.t;
    sequencer : Sequencer.t;
    length : Length_counter.t;
    sweep : Sweep.t;
    mutable duty_type : int;
    mutable enabled : bool;
    envelope : Envelope.t;
  }

  let create kind =
    {
      timer = Divider.create 0;
      sequencer = Sequencer.create 8;
      length = Length_counter.create ();
      duty_type = 0;
      sweep = Sweep.create kind;
      enabled = false;
      envelope = Envelope.create ();
    }

  let active t = Length_counter.active t.length

  let write0 t v =
    t.duty_type <- v lsr 6;
    let halt_loop = v land 0x20 <> 0 in
    let constant = v land 0x10 <> 0 in
    Envelope.set_constant t.envelope constant;
    Envelope.set_volume t.envelope (v land 0xF);
    Envelope.set_loop t.envelope halt_loop;
    Length_counter.update t.length halt_loop

  let update t v =
    t.enabled <- v;
    if not t.enabled then Length_counter.reset t.length

  let write1 t = Sweep.write t.sweep

  let write2 t v =
    let new_length = t.timer.length land 0x700 lor v in
    Divider.set_length t.timer new_length;
    Sweep.new_target t.sweep new_length

  let write3 t v =
    let new_length = t.timer.length land 0xFF lor ((v land 0x7) lsl 8) in
    Divider.set_length t.timer new_length;
    Sweep.new_target t.sweep new_length;
    Length_counter.load t.length (v lsr 3);
    Sequencer.reset t.sequencer;
    Envelope.set_start t.envelope

  let clock t = if Divider.clock t.timer then Sequencer.clock t.sequencer

  let frame_clock t =
    if t.enabled then Length_counter.clock t.length;
    Sweep.clock t.sweep;
    Divider.set_length t.timer t.sweep.current_period

  let output t =
    if
      t.timer.length >= 8
      && (not @@ Sweep.muted t.sweep)
      && Length_counter.active t.length
    then
      Envelope.output t.envelope
      * duties.(t.duty_type).(Sequencer.get t.sequencer)
    else 0
end

module Triangle = struct
  let sequence =
    [|
      15;
      14;
      13;
      12;
      11;
      10;
      9;
      8;
      7;
      6;
      5;
      4;
      3;
      2;
      1;
      0;
      0;
      1;
      2;
      3;
      4;
      5;
      6;
      7;
      8;
      9;
      10;
      11;
      12;
      13;
      14;
      15;
    |]

  type t = {
    mutable enabled : bool;
    timer : Divider.t;
    sequencer : Sequencer.t;
    length : Length_counter.t;
    linear_counter : Counter.t;
    mutable linear_reload : int;
    mutable control : bool;
  }

  let create () =
    {
      enabled = false;
      timer = Divider.create 0;
      sequencer = Sequencer.create 32;
      linear_counter = Counter.create ();
      length = Length_counter.create ();
      linear_reload = 0;
      control = false;
    }

  let active t = Length_counter.active t.length

  let update t v =
    t.enabled <- v;
    if not t.enabled then Length_counter.reset t.length

  let write_linear t v =
    let value = v land 0x7F in
    let control = v land 0x80 <> 0 in
    t.linear_reload <- value;
    Counter.load t.linear_counter value;
    Length_counter.update t.length control;
    t.control <- control

  let write_a t v =
    let new_length = t.timer.length land 0x700 lor v in
    Divider.set_length t.timer new_length

  let write_b t v =
    let new_length = t.timer.length land 0xFF lor ((v land 0x7) lsl 8) in
    Divider.set_length t.timer new_length;
    Length_counter.load t.length (v lsr 3);
    Sequencer.reset t.sequencer;
    t.linear_counter.halt <- false

  let clock t =
    if
      Divider.clock t.timer
      && Counter.active t.linear_counter
      && Length_counter.active t.length
    then Sequencer.clock t.sequencer

  let linear_frame_clock t =
    if t.linear_counter.halt then t.linear_counter.counter <- t.linear_reload
    else Counter.clock t.linear_counter;
    if not t.control then Counter.update t.linear_counter false

  let frame_clock t = if t.enabled then Length_counter.clock t.length
  let output t = sequence.(Sequencer.get t.sequencer)
end

(* Linear Feedback Shift Register *)
module LFSR = struct
  type t = { mutable register : U16.t; mutable mode : bool }

  let create () = { register = U16.one; mode = false }
  let set_mode t b = t.mode <- b

  let active t =
    let open U16 in
    t.register $& 1U = 0U

  let feedback t =
    let open U16 in
    let b0 = t.register $& 1U in
    let n = if t.mode then 6 else 1 in
    let bn = t.register $>> n $& 1U in
    b0 $^ bn = 1U

  let clock t =
    let open U16 in
    let feedback = feedback t in
    t.register <- t.register $>> 1;
    if feedback then t.register <- t.register $| 0x4000U
end

module Noise = struct
  type t = {
    length : Length_counter.t;
    timer : Divider.t;
    lfsr : LFSR.t;
    mutable enabled : bool;
    envelope : Envelope.t;
  }

  let create () =
    {
      length = Length_counter.create ();
      timer = Divider.create 0;
      lfsr = LFSR.create ();
      enabled = false;
      envelope = Envelope.create ();
    }

  let update t v =
    t.enabled <- v;
    if not t.enabled then Length_counter.reset t.length

  let active t = Length_counter.active t.length

  let periods =
    [|
      4; 8; 16; 32; 64; 96; 128; 160; 202; 254; 380; 508; 762; 1016; 2034; 4068;
    |]

  let write_c t v =
    let halt_loop = v land 0x20 <> 0 in
    let constant = v land 0x10 <> 0 in
    Envelope.set_constant t.envelope constant;
    Envelope.set_loop t.envelope halt_loop;
    Envelope.set_volume t.envelope (v land 0xF);
    Length_counter.update t.length halt_loop

  let write_e t v =
    Divider.set_length t.timer periods.(0xF land v);
    LFSR.set_mode t.lfsr (v land 0x80 <> 0)

  let write_f t v =
    Length_counter.load t.length (v lsr 3);
    Envelope.set_start t.envelope

  let frame_clock t = if t.enabled then Length_counter.clock t.length

  let output t =
    if LFSR.active t.lfsr && Length_counter.active t.length then
      Envelope.output t.envelope
    else 0

  let clock t = if Divider.clock t.timer then LFSR.clock t.lfsr
end

module DMC = struct
  type t = { mutable enabled : bool; mutable output_level : int }

  let create () = { enabled = false; output_level = 0 }
  let write0 _ _ = ()
  let write1 t v = t.output_level <- v land 0x7F
  let write2 _ _ = ()
  let write3 _ _ = ()
  let output t = t.output_level
end

module Frame_counter = struct
  module Event = struct
    type t = O | E | EL | ELF

    let is_e t = t <> O
    let is_l = function EL | ELF -> true | _ -> false
    let is_f t = t = ELF
  end

  let mode1 = Event.[| E; EL; E; ELF |]
  let mode2 = Event.[| EL; E; EL; E; O |]

  type t = {
    divider : Divider.t;
    sequencer : Sequencer.t;
    mutable mode : bool;
    mutable frame_interrupt : bool;
  }

  let create () =
    {
      divider = Divider.create cpu_divider;
      sequencer = Sequencer.create 4;
      mode = false;
      frame_interrupt = false;
    }

  let collector_id = "apu_frame_counter"

  let clear_interrupt t collector =
    t.frame_interrupt <- false;
    C6502.IRQ_collector.set_pulled collector collector_id false

  let write t collector v =
    Divider.reload t.divider;
    Sequencer.reset t.sequencer;
    let mode = v lsr 7 <> 0 in
    t.mode <- mode;
    if v lsr 6 <> 0 then clear_interrupt t collector;
    Sequencer.set_length t.sequencer (if mode then 5 else 4);
    Sequencer.clock t.sequencer

  let action t (pulse1, pulse2, triangle, noise) collector =
    let mode_array = if t.mode then mode2 else mode1 in
    let event = mode_array.(Sequencer.get t.sequencer) in
    if Event.is_e event then (
      Triangle.linear_frame_clock triangle;
      Envelope.clock Pulse.(pulse1.envelope);
      Envelope.clock Pulse.(pulse2.envelope);
      Envelope.clock Noise.(noise.envelope));
    if Event.is_l event then (
      Pulse.frame_clock pulse1;
      Pulse.frame_clock pulse2;
      Triangle.frame_clock triangle;
      Noise.frame_clock noise);
    t.frame_interrupt <- Event.is_f event;
    C6502.IRQ_collector.set_pulled collector collector_id t.frame_interrupt

  let clock t units collector =
    if Divider.clock t.divider then (
      Sequencer.clock t.sequencer;
      action t units collector)
end

module Resampler = struct
  type t = {
    sampling_freq : float;
    device : int32;
    mutable fb :
      (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
        (* frame buffer *)
    mutable fb_length : int;
    mutable history : float array; (* 4 last values *)
    mutable mu : float;
    mutable ratio : float;
    max_delta : float;
  }

  (* enough to store samples for two frames at 44100 Hz *)
  let fb_capacity = 2048

  let create sampling_freq device cli_flags =
    {
      device;
      fb = Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout fb_capacity;
      fb_length = 0;
      history = Array.make 4 0.;
      mu = 0.;
      sampling_freq = float_of_int sampling_freq;
      ratio = float_of_int cpu_freq /. float_of_int sampling_freq;
      max_delta = (if cli_flags.Common.uncap_speed then 0.5 else 0.005);
    }

  (* Return 0. if queue empty 1. if >= max_queue_size *)
  let fill_level t =
    let max_queue_size = 8192 in
    let current = min (Sdl.get_queued_audio_size t.device) max_queue_size in
    float_of_int current /. float_of_int max_queue_size

  let update_frequency t =
    let fill_level = fill_level t in
    let cpu_freq = float_of_int cpu_freq in
    let base_ratio = cpu_freq /. t.sampling_freq in
    let coef = 1. -. t.max_delta +. (2. *. fill_level *. t.max_delta) in
    let target_ratio = base_ratio *. coef in
    t.ratio <- target_ratio

  let append t value =
    if t.fb_length < fb_capacity then (
      t.fb.{t.fb_length} <- value;
      t.fb_length <- t.fb_length + 1)

  (* Cubic interpolation *)
  let buffer_next t value =
    t.history.(0) <- t.history.(1);
    t.history.(1) <- t.history.(2);
    t.history.(2) <- t.history.(3);
    t.history.(3) <- value;
    while t.mu <= 1.0 do
      let a =
        t.history.(3) -. t.history.(2) -. t.history.(0) +. t.history.(1)
      in
      let b = t.history.(0) -. t.history.(1) -. a in
      let c = t.history.(2) -. t.history.(0) in
      let d = t.history.(1) in
      append t
        ((a *. t.mu *. t.mu *. t.mu) +. (b *. t.mu *. t.mu) +. (c *. t.mu) +. d);
      t.mu <- t.mu +. t.ratio
    done;
    t.mu <- t.mu -. 1.0

  let resample t =
    let res = Bigarray.Array1.sub t.fb 0 t.fb_length in
    t.fb_length <- 0;
    if dynamic_rate_control then update_frequency t;
    res
end

module APU = struct
  type audio_backend = { device : int32; sampling_freq : int }

  type t = {
    frame_counter : Frame_counter.t;
    pulse1 : Pulse.t;
    pulse2 : Pulse.t;
    triangle : Triangle.t;
    resampler : Resampler.t;
    half_clock : Divider.t;
    noise : Noise.t;
    dmc : DMC.t;
    backend : audio_backend;
    collector : C6502.IRQ_collector.t;
  }

  let create backend collector cli_flags =
    {
      frame_counter = Frame_counter.create ();
      pulse1 = Pulse.create Sweep.Pulse1;
      pulse2 = Pulse.create Sweep.Pulse2;
      triangle = Triangle.create ();
      resampler =
        Resampler.create backend.sampling_freq backend.device cli_flags;
      half_clock = Divider.create 1;
      noise = Noise.create ();
      dmc = DMC.create ();
      collector;
      backend;
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
    | 0x400C -> Noise.write_c t.noise v
    | 0x400E -> Noise.write_e t.noise v
    | 0x400F -> Noise.write_f t.noise v
    | 0x4010 -> DMC.write0 t.dmc v
    | 0x4011 -> DMC.write1 t.dmc v
    | 0x4012 -> DMC.write2 t.dmc v
    | 0x4013 -> DMC.write3 t.dmc v
    | 0x4015 ->
        (* status *)
        let e_pulse1 = v land 0x1 <> 0 in
        let e_pulse2 = v land 0x2 <> 0 in
        let e_triangle = v land 0x4 <> 0 in
        let e_noise = v land 0x8 <> 0 in
        let _e_dmc = v land 0x10 <> 0 in
        Pulse.update t.pulse1 e_pulse1;
        Pulse.update t.pulse2 e_pulse2;
        Triangle.update t.triangle e_triangle;
        Noise.update t.noise e_noise
        (* TODO update other stuff *)
    | 0x4017 -> Frame_counter.write t.frame_counter t.collector v
    | _ -> ()

  let read_register t r =
    if Stdint.Uint16.to_int r = 0x4015 then (
      let iob n b = if b then 1 lsl n else 0 in
      let a_pulse1 = Pulse.active t.pulse1 |> iob 0 in
      let a_pulse2 = Pulse.active t.pulse1 |> iob 1 in
      let a_triangle = Triangle.active t.triangle |> iob 2 in
      let a_noise = Noise.active t.noise |> iob 3 in
      let interrupt = t.frame_counter.frame_interrupt |> iob 6 in
      Frame_counter.clear_interrupt t.frame_counter t.collector;
      a_pulse1 lor a_pulse2 lor interrupt lor a_triangle lor a_noise
      |> Stdint.Uint8.of_int
      (* TODO other bits *))
    else failwith "Read invalid APU register"

  let mixer t =
    let pulse1 = float_of_int @@ Pulse.output t.pulse1 in
    let pulse2 = float_of_int @@ Pulse.output t.pulse2 in
    let triangle = float_of_int @@ Triangle.output t.triangle in
    let noise = float_of_int @@ Noise.output t.noise in
    let dmc = float_of_int @@ DMC.output t.dmc in
    let pulse_out = 95.88 /. ((8128. /. (pulse1 +. pulse2)) +. 100.) in
    let tnd_factor =
      (triangle /. 8227.) +. (noise /. 12241.) +. (dmc /. 22638.)
    in
    let tnd_out = 159.79 /. ((1. /. tnd_factor) +. 100.) in
    let out_raw = pulse_out +. tnd_out in
    (* [0. - 1.] *)
    out_raw *. volume_modifier

  let next_cycle t =
    (* Clock pulse timers *)
    if Divider.clock t.half_clock then (
      Frame_counter.clock t.frame_counter
        (t.pulse1, t.pulse2, t.triangle, t.noise)
        t.collector;
      Pulse.clock t.pulse1;
      Pulse.clock t.pulse2;
      Noise.clock t.noise);
    Triangle.clock t.triangle;
    Resampler.buffer_next t.resampler (mixer t)

  let output_frame t =
    let buffer = Resampler.resample t.resampler in
    match Sdl.queue_audio t.backend.device buffer with
    | Ok () -> ()
    | Error (`Msg e) ->
        Printf.printf "Error when pushing samples: %s\n" e;
        assert false

  let exit t = Sdl.close_audio_device t.backend.device
end

include APU

let create collector =
  match Sdl.init Sdl.Init.audio with
  | Error (`Msg e) ->
      Printf.printf "Error while initializing audio device %s" e;
      assert false
  | Ok () ->
      ();
      let audio_spec =
        {
          Sdl.as_freq = 44100;
          as_format = Sdl.Audio.f32;
          as_channels = 1;
          as_silence = 0;
          as_samples = 1024;
          as_size = Int32.zero;
          as_callback = None;
        }
      in
      let dev, have =
        match
          Sdl.open_audio_device None false audio_spec
            Sdl.Audio.allow_frequency_change
        with
        | Error (`Msg e) ->
            Printf.printf "Error while opening audio device: %s\n" e;
            assert false
        | Ok h -> h
      in
      Sdl.pause_audio_device dev false;
      let backend = APU.{ sampling_freq = have.as_freq; device = dev } in
      APU.create backend collector
