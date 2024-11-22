(* Type of parsed input.
 *
 * This type lives in separate file to prevent dependency cycle between
 * SID and ParserContext.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open MemoryModel

module S := Set.Make(String)
module M := Map.Make(String)

type t = {
  (* Declarations *)
  sorts : Sort.t M.t;      (* Parametric sorts *)
  struct_defs : StructDef.t M.t;
  vars : Sort.t M.t;       (* Declared variables *)
  heap_sort : HeapSort.t;  (* Declared sort of the heap *)

  declared_preds : S.t;
  (** Context needs only names, actual definitions are registered in SID. *)

  (* Attributes *)
  expected_status : [ `Sat | `Unsat | `Unknown of string ];
  attributes : String.t M.t;

  (* Options *)
  produce_models : bool;

  (* Assertions *)
  assertions : SL.t list;
}
