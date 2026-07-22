(* Boolean logic.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open BaseLogic_terms

module Enumeration = struct

  include BaseLogic_equality.Smart
  include BaseLogic_vars

  let mk_sort = Sort.mk_finite

  let mk_const sort name = mk_constant (Constant.mk_const sort name)

  let get_constants sort =
    Sort.get_constant_names sort
    |> List.map (Constant.mk_const sort)

  let get_constants_terms sort =
    Sort.get_constant_names sort
    |> List.map (mk_const sort)

end
