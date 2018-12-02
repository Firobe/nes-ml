val memory : int array

val get_register : int -> int
val set_register : int -> int -> unit
val dma : int array -> int -> unit

val init : (unit -> unit) -> bool -> unit
val exit : unit -> unit

val dump_memory : unit -> unit
val next_cycle : unit -> unit

(* val debug_vram : int -> unit *)
