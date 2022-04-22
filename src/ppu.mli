open Stdint

module State : sig
  module Mem : sig
    val main : uint8 array
  end
end

val get_register : int -> uint8
val set_register : int -> uint8 -> unit
val dma : (uint16 -> uint8) -> uint16 -> unit

val init : (unit -> unit) -> bool -> Display.t
val exit : Display.t -> unit

val next_cycle : Display.t -> unit

module Debug : sig
  type t
  val init : unit -> t
  val delete : t -> unit
  val render : t option -> unit
end
