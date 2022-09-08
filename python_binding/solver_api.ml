(* Simplified result of solver
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Astral_lib
open StackHeapModel

open Python_types

open Python_lib

type result =
  | Sat of model
  | Unsat
  | Unknown of string
[@@deriving python]

let convert_model sh =
  let s = Stack.bindings sh.stack in
  let h = Heap.bindings sh.heap in
  (s, h)

let solve phi vars =
  let result = Api.solve phi vars in
  match result.status with
  | `SAT -> Sat (Obj.magic convert_model @@ Option.get result.model)
  | `UNSAT -> Unsat
  | `UNKNOWN -> Unknown ""
