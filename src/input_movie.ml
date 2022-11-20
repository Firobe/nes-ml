module type Movie = sig
  val file : string
end

module KSet = Set.Make (Input.Keys)
module ISet = Set.Make (Int)

let debug = false

let string_of_kset t =
  KSet.fold (fun e s -> Printf.sprintf "%s %s" s (Input.Keys.to_string e)) t ""

module Make_FM2 (M : Movie) : Input.Backend = struct
  type t = { mutable frame : int; inputs : KSet.t array }

  let create () = { frame = 0; inputs = Movie_format.FM2.Read.read M.file }

  let key_pressed t k =
    if t.frame >= Array.length t.inputs then failwith "End of movie";
    let set = t.inputs.(t.frame) in
    KSet.mem k set

  let get_inputs _ _ = ()

  let next_frame t =
    if debug then
      Printf.printf "%d: %s\n%!" t.frame (string_of_kset t.inputs.(t.frame));
    t.frame <- t.frame + 1
end

module Make_deter (M : Movie) : Input.Backend = struct
  type t = { mutable counter : int; inputs : ISet.t; last : int }

  let read_deter file =
    let chan = open_in file in
    let rec aux () =
      try
        let line = input_line chan in
        let i = int_of_string line in
        ISet.add i (aux ())
      with End_of_file -> ISet.empty
    in
    aux ()

  let create () =
    let inputs = read_deter M.file in
    { counter = 0; inputs; last = ISet.max_elt inputs }

  let key_pressed t _ =
    if t.counter > t.last then raise Common.End_of_movie;
    let r = ISet.mem t.counter t.inputs in
    t.counter <- t.counter + 1;
    r

  let get_inputs _ _ = ()
  let next_frame _ = ()
end
