module type Mmap = sig
    val read : int array -> int -> int
    val write : int array -> int -> int -> unit
end

module Make : functor (M : Mmap) -> sig
    val memory : int array
    val program_counter : int ref
    val stack_pointer : int ref
    val processor_status : int ref
    val acc : int ref
    val irx : int ref
    val iry : int ref
    val enable_decimal : bool ref
    val cycle_count : int ref

    val fetch_instr : unit -> unit
    val print_state : unit -> unit
    val reset : unit -> unit
    val init_pc : unit -> unit
    val interrupt : unit -> unit
end
