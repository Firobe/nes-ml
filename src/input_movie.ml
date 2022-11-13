module type Movie = sig
  val file : string
end

module KSet = Set.Make (Input.Keys)

let debug = false

let string_of_kset t =
  KSet.fold (fun e s -> Printf.sprintf "%s %s" s (Input.Keys.to_string e)) t ""

module Make (M : Movie) : Input.Backend = struct
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
