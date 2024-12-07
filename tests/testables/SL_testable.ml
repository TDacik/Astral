include SL.Infix

module T = struct
  include SL
  let equal = (===)
end

let (|~>) x y = SL_builtins.mk_ls x ~sink:y

include T
include Builder.Make(T)

(** Values *)
let nil = SL.Term.nil
let u = SL.Term.mk_var "u" Sort.loc_ls
let v = SL.Term.mk_var "v" Sort.loc_ls
let w = SL.Term.mk_var "w" Sort.loc_ls
let x = SL.Term.mk_var "x" Sort.loc_ls
let y = SL.Term.mk_var "y" Sort.loc_ls
let z = SL.Term.mk_var "z" Sort.loc_ls
let p1 = SL.of_smt @@ SMT.mk_var "p1" Sort.int
let p2 = SL.of_smt @@ SMT.mk_var "p2" Sort.int

module Var = struct
  include Builder.Make(SL.Variable)

  (** Values *)
  let nil = SL.Term.nil
  let u = SL.Term.mk_var "x" Sort.loc_ls
  let v = SL.Term.mk_var "y" Sort.loc_ls
  let w = SL.Term.mk_var "z" Sort.loc_ls
  let x = SL.Term.mk_var "x" Sort.loc_ls
  let y = SL.Term.mk_var "y" Sort.loc_ls
  let z = SL.Term.mk_var "z" Sort.loc_ls
end
