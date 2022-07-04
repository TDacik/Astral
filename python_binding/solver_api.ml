(** Simplified result of solver *)

open Astral_lib
open StackHeapModel

open PythonTypes

open Python_lib

type result =
  | SAT of model
  | UNSAT
  | UNKNOWN of string
[@@deriving python]

let convert_model sh =
  Printf.printf "%s\n" (to_string sh);
  let s = Stack.bindings sh.stack in
  let h = Heap.bindings sh.heap in
  (s, h)

let solve phi vars =
  let result = Solver.solve phi vars in
  match result.status with
  | `SAT -> SAT (Obj.magic convert_model @@ Option.get result.model)
  | `UNSAT -> UNSAT
  | `UNKNOWN -> UNKNOWN ""
