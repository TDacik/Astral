(* Quantifiers.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open BaseLogic_terms

module Boolean = BaseLogic_boolean.Smart (* TODO: split *)

module Quantifiers = struct

  (* TODO: smart constructors for binders *)

  let mk_forall vars ?ranges t =
    mk_binder (Forall ranges) vars t

  let mk_exists vars ?ranges t =
    if !BaseLogic_config.do_simplification then match t with
      | c when equal c Boolean.ff -> Boolean.ff
      | c when equal c Boolean.tt -> Boolean.tt
      | _ -> mk_binder (Exists ranges) vars t
    else mk_binder (Exists ranges) vars t

  let mk_exists' sorts constructor =
    let binders = List.map (Variable.mk_fresh "e") sorts in
    let terms = List.map (of_var) binders in
    mk_exists binders (constructor terms)

  let mk_forall' sorts constructor =
    let binders = List.map (Variable.mk_fresh "e") sorts in
    let terms = List.map (of_var) binders in
    mk_forall binders (constructor terms)


  let mk_forall2 vars ?ranges t =
    mk_binder (Forall2 ranges) vars t

  let mk_exists2 vars ?ranges t =
    mk_binder (Exists2 ranges) vars t

  let mk_forall2_range vars ranges t = mk_binder (Forall2 ranges) vars t
  let mk_exists2_range vars ranges t = mk_binder (Exists2 ranges) vars t

end
