module KSet = Set.Make (Input.Keys)

(** FM2 is a really bad file format: non determinist *)
module FM2 = struct
  module Read = struct
    let rec get_input_log chan =
      try
        let line = input_line chan in
        if line.[0] = '|' then line :: get_input_log chan
        else get_input_log chan
      with End_of_file -> []

    let parse_log_line line =
      let second_pipe = String.index_from line 1 '|' in
      let inputs = String.sub line (second_pipe + 1) 8 in
      let mapping =
        Input.Keys.[| Right; Left; Down; Up; Start; Select; B; A |]
      in
      let keys = ref KSet.empty in
      String.iteri
        (fun i c ->
          if c <> '.' && c <> ' ' then keys := KSet.add mapping.(i) !keys)
        inputs;
      !keys

    let read file =
      let chan = open_in file in
      let log = get_input_log chan in
      let log_parsed = List.map parse_log_line log in
      Array.of_list log_parsed
  end

  module Write = struct
    let write_header channel =
      Printf.fprintf channel
        {|version 3
emuVersion 20604
palFlag 0
romFilename ???
romChecksum ???
guid ???
fourscore 0
microphone 0
port0 1
port1 0
port2 0
FDS 0
NewPPU 0
|}

    let write_line channel ks =
      let p = Printf.fprintf channel in
      let pk k c =
        if KSet.mem k ks then Printf.fprintf channel "%c" c
        else Printf.fprintf channel "."
      in
      p "|0|";
      pk Right 'R';
      pk Left 'L';
      pk Down 'D';
      pk Up 'U';
      pk Start 'T';
      pk Select 'S';
      pk B 'B';
      pk A 'A';
      p "|||\n"
  end
end
