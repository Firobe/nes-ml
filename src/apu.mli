type t

val create : unit -> t

val next_cycle : t -> unit

val write_register : t -> Stdint.uint8 -> Stdint.uint16 -> unit

val read_register : t -> Stdint.uint16 -> Stdint.uint8

val exit : t -> unit
