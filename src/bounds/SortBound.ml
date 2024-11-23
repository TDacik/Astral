module Self = struct

  type t = {
    allocated : Int.t;
    total : Int.t;
  }

  let show bound = Format.asprintf "(allocated: %d, total: %d)" bound.allocated bound.total

end

include Self
include Datatype.Printable(Self)

let init allocated total = {allocated = allocated; total = total}

let zero = init 0 0

let n x = init x x

let plus b1 b2 = init (b1.allocated + b2.allocated) (b1.total + b2.total)
