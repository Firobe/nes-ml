(** NES cartridge mappers, as first-class modules *)

open Stdint

module type S' = sig
  type t

  val read : t -> uint16 -> uint8
  val write : t -> uint16 -> uint8 -> unit
end

module type S = sig
  type t

  val create : Rom.t -> t

  module CPU : S' with type t := t
  module PPU : S' with type t := t
end

val find : Rom.t -> (module S)
(** Try to find the implementation of the appropriate mapper for the given ROM,
    if it exists. Raises [Invalid_ROM] if the mapper is not implemented. *)
