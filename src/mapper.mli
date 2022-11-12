(** NES cartridge mappers, as first-class modules *)

module type S = C6502.MemoryMap with type input := Rom.t

val find : Rom.t -> (module S)
(** Try to find the implementation of the appropriate mapper for the given ROM,
    if it exists. Raises [Invalid_ROM] if the mapper is not implemented. *)
