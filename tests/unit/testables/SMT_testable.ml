module T = struct
  include SMT
  let equal = (===)
end

include T
include Builder.Make(T)

let x = SMT.mk_var "x" Sort.int
let y = SMT.mk_var "y" Sort.int
let z = SMT.mk_var "z" Sort.int

let set_sort = Sort.mk_set Sort.int
let s1 = SMT.mk_var "s1" set_sort
let s2 = SMT.mk_var "s2" set_sort
let s3 = SMT.mk_var "s3" set_sort

module Var = struct
  include Builder.Make(SMT.Variable)

  let x = SMT.Variable.mk "x" Sort.int
  let y = SMT.Variable.mk "y" Sort.int
  let z = SMT.Variable.mk "z" Sort.int
end
