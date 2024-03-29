(** Emulate and interface with a PPU chip *)

open Stdint

module type S = sig
  type t
  type mapper

  val create : mapper -> C6502.NMI.t -> t
  (** Create the chip from a fixed mirroring kind and a NMI channel to the CPU *)

  val frame : t -> int
  (** Current frame number *)

  val get_register : t -> int -> uint8
  (** Emulate reading from an address of the PPU *)

  val set_register : t -> int -> uint8 -> unit
  (** Same, for writing *)

  val dma : t -> (uint16 -> uint8) -> uint16 -> unit
  (** Direct Memory Access: given the PPU, a function to read from CPU addresses
      and a starting PPU address, blit the memory *)

  val next_cycle : t -> Common.set_pixel -> unit
  (** Emulate next cycle of the PPU *)

  val should_render : t -> uint8 option

  (** Create and destroy the windows dedicated to the PPU debugging *)
  module Debug : sig
    type ppu := t
    type t

    val create : unit -> t
    val delete : t -> unit
    val render : ppu -> t option -> unit
  end
end

module Make (M : Mapper.S) : S with type mapper = M.t
