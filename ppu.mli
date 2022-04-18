open Stdint

val memory : uint8 array

val get_register : int -> uint8
val set_register : int -> uint8 -> unit
val dma : (uint16 -> uint8) -> uint16 -> unit

val init : (unit -> unit) -> bool -> unit
val exit : unit -> unit

(* val dump_memory : unit -> unit *)
val next_cycle : Display.t -> unit

(* val debug_vram : int -> unit *)
