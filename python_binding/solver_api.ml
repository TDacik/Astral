(* Simplified result of solver
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Astral_lib
open StackHeapModel

open Python_lib

type result = [`Sat | `Unsat | `Unknown of string] [@@deriving python]

let convert_model sh =
  let s = Stack.bindings sh.stack in
  let h = Heap.bindings sh.heap in
  (s, h)

let solve phi vars = Api.solve phi vars
