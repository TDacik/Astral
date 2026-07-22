open BaseLogic_terms

let mk_const c = mk_constant (Constant.mk_bool c)
let tt = mk_const true
let ff = mk_const false
