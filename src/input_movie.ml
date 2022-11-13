module type Movie = sig
  val file : string
end

module KSet = Set.Make (Input.Keys)

let debug = false

let string_of_kset t =
  KSet.fold (fun e s -> Printf.sprintf "%s %s" s (Input.Keys.to_string e)) t ""

let rec fm2_log_lines chan =
  try
    let line = input_line chan in
    if line.[0] = '|' then line :: fm2_log_lines chan else fm2_log_lines chan
  with End_of_file -> []

let fm2_parse_log_line line =
  let second_pipe = String.index_from line 1 '|' in
  let inputs = String.sub line (second_pipe + 1) 8 in
  let mapping = Input.Keys.[| Right; Left; Down; Up; Start; Select; B; A |] in
  let keys = ref KSet.empty in
  String.iteri
    (fun i c -> if c <> '.' && c <> ' ' then keys := KSet.add mapping.(i) !keys)
    inputs;
  !keys

let read_fm2 file =
  let chan = open_in file in
  let log = fm2_log_lines chan in
  let log_parsed = List.map fm2_parse_log_line log in
  Array.of_list log_parsed

module Make (M : Movie) : Input.Backend = struct
  type t = { mutable frame : int; inputs : KSet.t array }

  let create () = { frame = 0; inputs = read_fm2 M.file }

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
