module Make (M : Stdint.Int) = struct
  include M
  let ($&) = M.logand
  let ($|) = M.logor
  let ($^) = M.logxor
  let ($>>) = M.shift_right_logical
  let ($>>!) = M.shift_right
  let ($<<) = M.shift_left
  let (?~) = M.lognot
  let (?$) = M.of_uint8
  let (?$$) = M.of_uint16
  let (?%) = M.to_int
  let (?@) = M.of_int
end

module Common = struct
  module U8 = Make(Stdint.Uint8)
  module U16 = Make(Stdint.Uint16)
end
