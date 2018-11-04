(* STATE *)

(* 0x000 to 0xFFFF main memory *)
let memory = Array.make 0x10000 0x00
let enable_decimal = ref true

(* Registers *)
let program_counter = ref 0x0400
let stack_pointer = ref 0x00FF
let accumulator = ref 0x00
let index_register_x = ref 0x00
let index_register_y = ref 0x00
let processor_status = ref 0x24

let cycle_count = ref 0

let reset () =
    Array.fill memory 0 0x10000 0x0 ;
    enable_decimal := true ;
    program_counter := 0x0400 ;
    accumulator := 0x0 ;
    index_register_x := 0x0 ;
    index_register_y := 0x0 ;
    processor_status := 0x24 ;
    cycle_count := 0

let init_pc () =
    program_counter := (memory.(0xFFFD) lsl 8) lor memory.(0xFFFC)

let is_in_ppu_range addr = 
    let prefix = addr lsr 13 in
    prefix = 1

(* Memory wrappers *)
class virtual memory_wrapper () = object(_)
    method virtual get : unit -> int
    method virtual set : int -> unit
    method virtual self : unit -> int
end
class addr_wrapper addr = object(_)
    inherit memory_wrapper ()
    val baddr = addr land 0xFFFF
    method get () =
        if is_in_ppu_range baddr then
            Ppu.get_register baddr
        else memory.(baddr)
    method set v =
        if is_in_ppu_range baddr then
            Ppu.set_register baddr v
        else if baddr = 0x4014 then (
            let cpu_begin = v lsl 8 in
(*             Printf.printf "OAM DMA 0x%X\n" cpu_begin; *)
            Array.blit memory cpu_begin Ppu.oam
                !Ppu.oam_address 0x100
        )
        else memory.(baddr) <- v
    method self () = addr
end
class ref_wrapper r = object(_)
    inherit memory_wrapper ()
    method get () = !r
    method set v = r := v
    method self () = assert false
end
class dummy_wrapper () = object(_)
    inherit memory_wrapper ()
    method get () = assert false
    method set _ = assert false
    method self () = assert false
end

(* Utils *)
type addressing_mode =
  | Implicit
  | Accumulator
  | Immediate
  | Zero_Page
  | Zero_Page_X
  | Zero_Page_Y
  | Relative
  | Absolute
  | Absolute_X
  | Absolute_Y
  | Indirect
  | Indexed_Indirect
  | Indirect_Indexed

let addressing_mode_size = function
  | Implicit
  | Accumulator       -> 1
  | Immediate
  | Zero_Page
  | Zero_Page_X
  | Zero_Page_Y
  | Indexed_Indirect
  | Indirect_Indexed
  | Relative          -> 2
  | Absolute
  | Absolute_X
  | Absolute_Y
  | Indirect          -> 3

let get_flag_mask f = match f with
  | `Carry     -> 1 lsl 0
  | `Zero      -> 1 lsl 1
  | `Interrupt -> 1 lsl 2
  | `Decimal   -> 1 lsl 3
  | `Break     -> 1 lsl 4
  | `Reserved  -> 1 lsl 5
  | `Overflow  -> 1 lsl 6
  | `Negative  -> 1 lsl 7

let set_flag cond f =
  let mask = get_flag_mask f in
  if cond then
    processor_status := !processor_status lor mask
  else
    processor_status := !processor_status land (lnot mask)

let get_flag f =
  if get_flag_mask f land !processor_status != 0 then 1 else 0

let update_zero_flag v =
  set_flag (v = 0) `Zero

let update_negative_flag v =
  let cond = (v lsr 7) land 0x1 = 1 in
  set_flag cond `Negative

let update_NZ v =
    update_zero_flag v ;
    update_negative_flag v

(* INSTRUCTIONS *)

(* Load/Store *)
let aux_LD r (m : memory_wrapper) =
  let v = m#get () in
  r := v ;
  update_NZ v

type instruction = {
  f : memory_wrapper -> unit;
  name : string
}

let gen_instr n f = {f=f; name=n}

let _LDA = gen_instr "LDA" @@ aux_LD accumulator
let _LDX = gen_instr "LDX" @@ aux_LD index_register_x
let _LDY = gen_instr "LDY" @@ aux_LD index_register_y

let aux_ST (m: memory_wrapper) v =
  m#set v

let _STA = gen_instr "STA" @@ fun m -> aux_ST m !accumulator
let _STX = gen_instr "STX" @@ fun m -> aux_ST m !index_register_x
let _STY = gen_instr "STY" @@ fun m -> aux_ST m !index_register_y
let _SAX = gen_instr "SAX" @@ fun m -> aux_ST m (!accumulator land !index_register_x)

(* Register transfers *)
let aux_T f t = 
  t := f ; update_NZ f

let _TAX = gen_instr "TAX" @@ fun _ -> aux_T !accumulator index_register_x
let _TAY = gen_instr "TAY" @@ fun _ -> aux_T !accumulator index_register_y
let _TXA = gen_instr "TXA" @@ fun _ -> aux_T !index_register_x accumulator
let _TYA = gen_instr "TYA" @@ fun _ -> aux_T !index_register_y accumulator

(* Stack operations *)
let aux_push v =
  memory.(0x0100 + !stack_pointer) <- v ;
  stack_pointer := (!stack_pointer - 1) land 0xFF

let aux_pull r =
  stack_pointer := (!stack_pointer + 1) land 0xFF ;
  r := memory.(0x0100 + !stack_pointer)

let _TSX = gen_instr "TSX" @@ fun _ -> aux_T !stack_pointer index_register_x
let _TXS = gen_instr "TXS" @@ fun _ -> stack_pointer := !index_register_x
let _PHA = gen_instr "PHA" @@ fun _ -> aux_push !accumulator
let _PHP = gen_instr "PHP" @@ fun _ -> aux_push (!processor_status lor get_flag_mask `Break)
let _PLA = gen_instr "PLA" @@ fun _ -> aux_pull accumulator ; update_NZ !accumulator
let _PLP = gen_instr "PLP" @@ fun _ ->
    aux_pull processor_status ;
    set_flag false `Break ;
    set_flag true `Reserved

(* Logical *)
let aux_logic f =
    accumulator := f !accumulator; update_NZ !accumulator

let _AND = gen_instr "AND" @@ fun m -> aux_logic ((land) @@ m#get ())
let _EOR = gen_instr "EOR" @@ fun m -> aux_logic ((lxor) @@ m#get ())
let _ORA = gen_instr "ORA" @@ fun m -> aux_logic ((lor) @@ m#get ())
let _BIT = gen_instr "BIT" @@ fun m -> 
  let v = m#get () in
  let masked = !accumulator land v in
  update_zero_flag masked ;
  set_flag ((v lsr 7) land 0x1 = 1) `Negative ;
  set_flag ((v lsr 6) land 0x1 = 1) `Overflow

(* Arithmetic *)
let bcd_to_dec b = 
    let lo = b land 0x0F in
    let hi = b lsr 4 in
    lo + hi * 10
let dec_to_bcd d =
    let lo = d mod 10 in
    let hi = d / 10 in
    lo lor (hi lsl 4)

(* Addition : binary or decimal *)
let _ADC = gen_instr "ADC" @@ fun m -> 
  let v = m#get () in
  let decimal = get_flag `Decimal = 1 && !enable_decimal in
  let pre = if decimal then bcd_to_dec else fun x -> x in
  let post = if decimal then dec_to_bcd else fun x -> x in
  let max = if decimal then 99 else 0xFF in
  let op1 = pre !accumulator in
  let op2 = pre v in
  let c = get_flag `Carry in
  let sum = op1 + op2 + c in
  let nc = sum > max in
  set_flag nc `Carry ;
  let rsum = sum mod (max + 1) in
  let overflow = ((!accumulator lxor rsum) land (v lxor rsum) land 0x80) != 0 in
  set_flag overflow `Overflow ;
  accumulator := post rsum ;
  update_NZ !accumulator

(* Subtraction : binary or decimal *)
let _SBC = gen_instr "SBC" @@ fun m -> 
  let v = m#get () in
  let c2 = if get_flag `Decimal = 1 && !enable_decimal then
      dec_to_bcd (100 - (bcd_to_dec v) - 1)
  else
      (v lxor 0xFF) in
  _ADC.f (new ref_wrapper (ref c2))

let aux_CMP r (m : memory_wrapper) =
  let nv = m#get () in
  let c = r - nv in
  update_zero_flag c ;
  update_negative_flag c ;
  set_flag (c >= 0) `Carry

let _CMP = gen_instr "CMP" @@ fun m -> aux_CMP !accumulator m
let _CPX = gen_instr "CPX" @@ fun m -> aux_CMP !index_register_x m
let _CPY = gen_instr "CPY" @@ fun m -> aux_CMP !index_register_y m

(* Increments & Decrements *)
let aux_cr v r =
  let nv = r#get () in
  let updated = (nv + v) land 0xFF in
  r#set updated ;
  update_NZ updated

let _INC = gen_instr "INC" @@ aux_cr 1
let _INX = gen_instr "INX" @@ fun _ -> _INC.f (new ref_wrapper index_register_x)
let _INY = gen_instr "INY" @@ fun _ -> _INC.f (new ref_wrapper index_register_y)
let _DEC = gen_instr "DEC" @@ aux_cr (-1)
let _DEX = gen_instr "DEX" @@ fun _ -> _DEC.f (new ref_wrapper index_register_x)
let _DEY = gen_instr "DEY" @@ fun _ -> _DEC.f (new ref_wrapper index_register_y)

(* Shifts *)
let aux_shift m r f =
    let nv = m#get () in
    let v = f nv in
    set_flag ((nv lsr r) land 0x1 = 1) `Carry ;
    m#set v ;
    update_NZ v

let _ASL = gen_instr "ASL" @@ fun m ->
  aux_shift m 7 @@ fun nv -> (nv lsl 1) land 0xFF
let _LSR = gen_instr "LSR" @@ fun m ->
  aux_shift m 0 @@ fun nv -> nv lsr 1
let _ROL = gen_instr "ROL" @@ fun m ->
  aux_shift m 7 @@ fun nv -> ((nv lsl 1) lor (get_flag `Carry)) land 0xFF
let _ROR = gen_instr "ROR" @@ fun m ->
  aux_shift m 0 @@ fun nv -> (nv lsr 1) lor ((get_flag `Carry) lsl 7)

(* Jump and calls *)
let _JMP =
  gen_instr "JMP" @@ fun m -> program_counter := m#self ()
let _JSR = gen_instr "JSR" @@ fun m ->
    let r = !program_counter - 1 in
    aux_push @@ r lsr 8 ;
    aux_push @@ r land 0x00FF ;
    _JMP.f m
let _RTS = gen_instr "RTS" @@ fun _ ->
    let hi = ref 0 in
    let lo = ref 0 in
    aux_pull lo ;
    aux_pull hi ;
    program_counter := !lo lor (!hi lsl 8) ;
    incr program_counter

(* Branches *)
let aux_branch f s v =
  let nv = if v > 0x7F then
      - (((lnot v) + 1) land 0xFF)
      else v
  in
  let nnv = (!program_counter + nv) land 0xFFFF in
  if get_flag f = s then (
      let cp = if (nnv land 0xFF00) != (!program_counter land 0xFF00)
        then 1 else 0 in
      cycle_count := !cycle_count + 1 + cp ;
    program_counter := nnv
  )

let _BCC = gen_instr "BCC" @@ fun rel -> aux_branch `Carry 0 @@ rel#get ()
let _BCS = gen_instr "BCS" @@ fun rel -> aux_branch `Carry 1 @@ rel#get ()
let _BEQ = gen_instr "BEQ" @@ fun rel -> aux_branch `Zero 1 @@ rel#get ()
let _BMI = gen_instr "BMI" @@ fun rel -> aux_branch `Negative 1 @@ rel#get ()
let _BNE = gen_instr "BNE" @@ fun rel -> aux_branch `Zero 0 @@ rel#get ()
let _BPL = gen_instr "BPL" @@ fun rel -> aux_branch `Negative 0 @@ rel#get ()
let _BVC = gen_instr "BVC" @@ fun rel -> aux_branch `Overflow 0 @@ rel#get ()
let _BVS = gen_instr "BVS" @@ fun rel -> aux_branch `Overflow 1 @@ rel#get ()

(* Status Flag Changes *)
let _CLC = gen_instr "CLC" @@ fun _ -> set_flag false `Carry
let _CLD = gen_instr "CLD" @@ fun _ ->  set_flag false `Decimal
let _CLI = gen_instr "CLI" @@ fun _ ->  set_flag false `Interrupt
let _CLV = gen_instr "CLV" @@ fun _ ->  set_flag false `Overflow
let _SEC = gen_instr "SEC" @@ fun _ ->  set_flag true `Carry
let _SED = gen_instr "SED" @@ fun _ ->  set_flag true `Decimal
let _SEI = gen_instr "SEI" @@ fun _ ->  set_flag true `Interrupt

let _UNO = gen_instr "UNF" @@ fun _ -> ()

(* System functions *)
let _BRK = gen_instr "BRK" @@ fun _ ->
  let v = !program_counter + 1 in
  aux_push @@ v lsr 8 ;
  aux_push @@ v land 0x00FF ;
  set_flag true `Break ;
  aux_push !processor_status ;
  set_flag true `Interrupt ;
  let irq_l = memory.(0xFFFE) in
  let irq_h = memory.(0xFFFF) lsl 8 in
  program_counter := irq_l lor irq_h

let _NOP = gen_instr "NOP" @@ fun _ -> ()

let _RTI = gen_instr "RTI" @@ fun _ ->
  aux_pull processor_status ;
  set_flag false `Break ;
  set_flag true `Reserved ;
  let lo = ref 0 in
  let hi = ref 0 in
  aux_pull lo ;
  aux_pull hi ;
  program_counter := !lo lor (!hi lsl 8)

let shift_and_mask v dec mask =
    let target = v lsr dec in
    target land mask

let invalid_instruction a b c =
    Printf.printf "Invalid instruction %.2X %d %d %d\n" memory.(!program_counter)
        a b c ;
    assert false

(* Addressing and instruction dispatch *)
let rec get_addressing_mode a b c =
    if c = 3 then (
        if b = 5 || b = 7 then
            get_addressing_mode a b (c-1)
        else
            get_addressing_mode a b (c-2)
    )
    else match b with
  | 0 -> begin match c with
      | 0 -> begin match a with
          | 1 -> Absolute
          | a when a >= 4 -> Immediate
          | _ -> Implicit
        end
      | 1 -> Indexed_Indirect
      | 2 -> Immediate
      | _ -> invalid_instruction a b c
    end
  | 1 -> Zero_Page
  | 2 -> begin match c with
      | 0 -> Implicit
      | 1 -> Immediate
      | 2 -> if a < 4 then Accumulator else Implicit
      | _ -> invalid_instruction a b c
    end
  | 3 -> if a = 3 && c = 0 then Indirect else Absolute
  | 4 -> begin match c with
      | 0 -> Relative
      | 1 -> Indirect_Indexed
      | _ -> invalid_instruction a b c
    end
  | 5 -> if a < 4 || a > 5 || c != 2 then Zero_Page_X else Zero_Page_Y
  | 6 -> begin match c with
      | 0 -> Implicit
      | 1 -> Absolute_Y
      | 2 -> Implicit
      | _ -> invalid_instruction a b c
    end
  | 7 -> if c = 2 && a = 5 then Absolute_Y else Absolute_X
  | _ -> invalid_instruction a b c

let rec get_instruction_fun a b c = match (c, a) with
    | 0, 0 -> List.nth [_BRK; _UNO; _PHP; _UNO; _BPL; _UNO; _CLC; _UNO] b
    | 0, 1 -> List.nth [_JSR; _BIT; _PLP; _BIT; _BMI; _UNO; _SEC; _UNO] b
    | 0, 2 -> List.nth [_RTI; _UNO; _PHA; _JMP; _BVC; _UNO; _CLI; _UNO] b
    | 0, 3 -> List.nth [_RTS; _UNO; _PLA; _JMP; _BVS; _UNO; _SEI; _UNO] b
    | 0, 4 -> List.nth [_UNO; _STY; _DEY; _STY; _BCC; _STY; _TYA; _UNO] b
    | 0, 5 -> List.nth [_LDY; _LDY; _TAY; _LDY; _BCS; _LDY; _CLV; _LDY] b
    | 0, 6 -> List.nth [_CPY; _CPY; _INY; _CPY; _BNE; _UNO; _CLD; _UNO] b
    | 0, 7 -> List.nth [_CPX; _CPX; _INX; _CPX; _BEQ; _UNO; _SED; _UNO] b
    | 1, 0 -> _ORA
    | 1, 1 -> _AND
    | 1, 2 -> _EOR
    | 1, 3 -> _ADC
    | 1, 4 -> if b = 2 then _NOP else _STA
    | 1, 5 -> _LDA
    | 1, 6 -> _CMP
    | 1, 7 -> _SBC
    | 2, 0 -> List.nth [_NOP; _ASL; _ASL; _ASL; _NOP; _ASL; _NOP; _ASL] b
    | 2, 1 -> List.nth [_NOP; _ROL; _ROL; _ROL; _NOP; _ROL; _NOP; _ROL] b
    | 2, 2 -> List.nth [_NOP; _LSR; _LSR; _LSR; _NOP; _LSR; _NOP; _LSR] b
    | 2, 3 -> List.nth [_NOP; _ROR; _ROR; _ROR; _NOP; _ROR; _NOP; _ROR] b
    | 2, 4 -> List.nth [_NOP; _STX; _TXA; _STX; _NOP; _STX; _TXS; _UNO] b
    | 2, 5 -> List.nth [_LDX; _LDX; _TAX; _LDX; _LDX; _LDX; _TSX; _LDX] b
    | 2, 6 -> List.nth [_NOP; _DEC; _DEX; _DEC; _DEC; _DEC; _NOP; _DEC] b
    | 2, 7 -> List.nth [_NOP; _INC; _NOP; _INC; _NOP; _INC; _NOP; _INC] b
    (* Unofficial *)
    | 3, 4 when b = 0 || b = 1 || b = 3 || b = 5  -> _SAX
    | 3, 0 when b != 2  && b != 1 -> get_instruction_fun a 1 c
    | 3, 1 when b != 2  && b != 1 -> get_instruction_fun a 1 c
    | 3, 2 when b != 2  && b != 1 -> get_instruction_fun a 1 c
    | 3, 3 when b != 2  && b != 1 -> get_instruction_fun a 1 c
    | 3, 6 when b != 2  && b != 1 -> get_instruction_fun a 1 c
    | 3, 7 when b != 2 && b != 1 -> get_instruction_fun a 1 c
    | 3, _ ->
        let f1 = (get_instruction_fun a b (c-1)).f in
        let f2 = (get_instruction_fun a b (c-2)).f in
        gen_instr "UNF" @@ fun m -> f1 m; f2 m
    | _ -> assert false

let get_instr_length ins mode page_crossed a b c = 
  let sup = ref 0 in
  let rec get_template ins page_crossed a b c = match ins.name with
    (* Impl, Acc, Imm, ZP, ZPX, ZPY, RLT, ABS, ABSX, ABSY, IND, INDX, INDY *)
    | "AND" | "EOR" | "ORA" | "BIT" | "ADC" | "SBC"
    | "CMP" | "CPX" | "CPY"
    | "LDA" | "LDX" | "LDY" ->
      if mode = Absolute_X || mode = Absolute_Y || mode = Indirect_Indexed then
        sup := if page_crossed then 1 else 0 ;
                                [0; 0; 2; 3; 4; 4; 0; 4; 4; 4; 0; 6; 5]
    | "STA" | "STX"
    | "SAX" | "STY" ->          [0; 0; 0; 3; 4; 4; 0; 4; 5; 5; 0; 6; 6]
    | "TAX" | "TAY" | "TXA"
    | "INX" | "INY" | "DEX"
    | "DEY" | "CLC" | "CLD"
    | "CLI" | "CLV" | "SEC"
    | "SED" | "SEI" | "NOP"
    | "TYA" | "TSX" | "TXS" ->  [2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
    | "PHA" | "PHP" ->          [3; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
    | "PLA" | "PLP" ->          [4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
    | "JSR" | "RTS" | "RTI" ->  [6; 0; 0; 0; 0; 0; 0; 6; 0; 0; 0; 0; 0]
    | "BRK" ->                  [7; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
    | "INC" | "DEC" | "ASL"
    | "LSR" | "ROR" | "ROL" ->  [0; 2; 0; 5; 6; 0; 0; 6; 7; 0; 0; 0; 0]
    | "JMP" ->                  [0; 0; 0; 0; 0; 0; 0; 3; 0; 0; 5; 0; 0]
    | "BCC" | "BCS" | "BEQ"
    | "BMI" | "BNE" | "BPL"
    | "BVC" | "BVS" ->          [0; 0; 0; 0; 0; 0; 2; 0; 0; 0; 0; 0; 0]
    | "UNF" when c = 3 &&
        (a >= 6 || a <= 3) ->   [0; 0; 2; 5; 6; 0; 0; 6; 7; 7; 0; 8; 8]
    | "UNF" ->
            let equiv = get_instruction_fun 5 b (if c >= 2 then c - 2 else 0) in
            get_template equiv page_crossed a b c
    | _ -> assert false
  in let template = get_template ins page_crossed a b c in
  let v = match mode with
      | Implicit -> 0 | Accumulator -> 1 | Immediate -> 2
      | Zero_Page -> 3 | Zero_Page_X -> 4 | Zero_Page_Y -> 5
      | Relative -> 6 | Absolute -> 7 | Absolute_X -> 8
      | Absolute_Y -> 9 | Indirect -> 10 | Indexed_Indirect -> 11
      | Indirect_Indexed -> 12
  in (List.nth template v) + !sup

let print_state () =
    let opcode = memory.(!program_counter) in
    let a = shift_and_mask opcode 5 0x7 in
    let b = shift_and_mask opcode 2 0x7 in
    let c = shift_and_mask opcode 0 0x3 in
    let size = addressing_mode_size @@ get_addressing_mode a b c in
    Printf.printf "%.4X  " !program_counter ;
    for i = 0 to size - 1 do Printf.printf "%.2X " memory.(!program_counter + i) done ;
    let name = (get_instruction_fun a b c).name in
    Printf.printf "\t%s" name ;
    Printf.printf "\t\t A:%.2X X:%.2X Y:%.2X P:%.2X SP:%.2X CYC:%3d\n%!"
        !accumulator !index_register_x !index_register_y !processor_status
        !stack_pointer (!cycle_count*3 mod 341)

let get_page v = v lsr 8

let interrupt () =
    let v = !program_counter in
    aux_push @@ v lsr 8 ;
    aux_push @@ v land 0x00FF ;
    aux_push !processor_status ;
    set_flag true `Interrupt ;
    let irq_l = memory.(0xFFFA) in
    let irq_h = memory.(0xFFFB) lsl 8 in
    program_counter := irq_l lor irq_h

let dump_memory () =
    let file = open_out_bin "memdump" in
    let store = Bytes.create 0x10000 in
    for i = 0 to (Array.length memory) - 1 do
        Bytes.set store i @@ char_of_int memory.(i)
    done ;
    output file store 0 (Bytes.length store) ;
    close_out file

let fetch_instr () =
  let opcode = memory.(!program_counter) in
  let a = shift_and_mask opcode 5 0x7 in
  let b = shift_and_mask opcode 2 0x7 in
  let c = shift_and_mask opcode 0 0x3 in
  let b1 = !program_counter + 1 in
  let b2 = !program_counter + 2 in
  let v1 = memory.(b1) in
  let v2 = memory.(b2) in
  let v12 = v1 lor (v2 lsl 8) in
  let addr_mode = get_addressing_mode a b c in
  let mode_size = addressing_mode_size addr_mode in
  program_counter := !program_counter + mode_size ;
  let page_crossed = ref false in
  let arg = begin match addr_mode with
  | Implicit -> new dummy_wrapper ()
  | Accumulator -> new ref_wrapper accumulator
  | Immediate -> new addr_wrapper b1
  | Zero_Page -> new addr_wrapper v1
  | Zero_Page_X -> new addr_wrapper ((v1 + !index_register_x) land 0xFF)
  | Zero_Page_Y -> new addr_wrapper ((v1 + !index_register_y) land 0xFF)
  | Relative -> new addr_wrapper b1
  | Absolute -> new addr_wrapper v12
  | Absolute_X -> 
        if get_page v12 != get_page (!index_register_x + v12) then
          page_crossed := true ;
        new addr_wrapper (!index_register_x + v12)
  | Absolute_Y ->
        if get_page v12 != get_page (!index_register_y + v12) then
          page_crossed := true ;
    new addr_wrapper (!index_register_y + v12)
  | Indirect ->
          (* Second byte of target wrap around in page *)
          let sto_addr_hi = ((v12 + 1) land 0xFF) lor (v12 land 0xFF00) in
          new addr_wrapper (memory.(v12) lor (memory.(sto_addr_hi) lsl 8))
  | Indexed_Indirect (*X*) -> 
          let sto_addr = (v1 + !index_register_x) land 0xFF in
          (* Second byte of target wrap around in zero page *)
          let sto_addr_hi = (sto_addr + 1) land 0xFF in
          let sto = memory.(sto_addr) lor (memory.(sto_addr_hi) lsl 8) in
          new addr_wrapper sto
  | Indirect_Indexed (*Y*) ->
          (* Second byte of target wrap around in zero page *)
        let sto_addr_hi = (v1 + 1) land 0xFF in
        let sto = memory.(v1) lor (memory.(sto_addr_hi) lsl 8) in
        if get_page sto != get_page (!index_register_y + sto) then
            page_crossed := true ;
        new addr_wrapper (sto + !index_register_y)
  end in
  let ins_fun = get_instruction_fun a b c in
  let cycles = get_instr_length ins_fun addr_mode !page_crossed a b c in
  cycle_count := !cycle_count + cycles ;
  (* Reserved bit always on *) 
  set_flag true `Reserved ;
  ins_fun.f arg

