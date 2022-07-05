(** Emulate and interface with an APU chip.

The emulated APU will output sound automatically when cycled through. The sound
will be fine only if the chip is cycled through at the appropriate frequency. 
*)

type t

val create : C6502.IRQ_collector.t -> t
(** Create the emulated chip, given a collector to communicate IRQs with the
 * CPU. Also initialize the sound backend. *)

val next_cycle : t -> unit
(** Emulate next cycle of the chip *)

val write_register : t -> Stdint.uint8 -> Stdint.uint16 -> unit
(** Emulate writing a value at an address of the APU *)

val read_register : t -> Stdint.uint16 -> Stdint.uint8
(** Similar, for reading *)

val exit : t -> unit
(** Release the sound backend *)
