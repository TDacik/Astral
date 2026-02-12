module T = struct
  include BaseLogic
  let equal = (===)
end

include T
include Builder.Make(T)

module Var = struct
  let x = Variable.mk "x" Sort.loc_ls
  let y = Variable.mk "y" Sort.loc_ls
  let z = Variable.mk "z" Sort.loc_ls
end

let x = BaseLogic.of_var Var.x
let y = BaseLogic.of_var Var.y
let z = BaseLogic.of_var Var.z
