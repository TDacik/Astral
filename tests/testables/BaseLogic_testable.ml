module T = struct
  include BaseLogic
  let equal = (===)
end

include T
include Builder.Make(T)

let x = BaseLogic.mk_var "x" Sort.int
let y = BaseLogic.mk_var "y" Sort.int
let z = BaseLogic.mk_var "z" Sort.int
