module type S = (C6502.MemoryMap with type input := Rom.t)

val find : Rom.t -> (module S)
