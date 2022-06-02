open Stdint

type t
type mirroring_kind =
  | Horizontal (* vertical arrangement *)
  | Vertical (* horizontal arrangement *)
  | Single
  | Quad

val create : mirroring_kind -> C6502.NMI.t -> t
val init_memory : t -> uint8 array -> int -> unit

val frame : t -> int
val get_register : t -> int -> uint8
val set_register : t -> int -> uint8 -> unit
val dma : t -> (uint16 -> uint8) -> uint16 -> unit
val next_cycle : t -> Display.t -> unit

module Window : sig
  val create : unit -> Display.t
  val exit : Display.t -> unit
end

module Debug : sig
  type ppu := t
  type t
  val create : unit -> t
  val delete : t -> unit
  val render : ppu -> t option -> unit
end
