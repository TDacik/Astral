(* Imperative internal state of BaseLogic (used for debugging).
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

let do_simplification = ref true

let use_simplification flag = do_simplification := flag
