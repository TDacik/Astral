(* Solver's public API
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

let solve phi vars =
  (* Create input *)
  let input =
    Context.empty
    |> Context.add_assertion phi
    |> Context.add_variables vars
  in
  let result = Solver.solve input in
  match Option.get result.status with
  | `Sat -> `Sat (Option.get result.model)
  | `Unsat -> `Unsat
  | `Unknown -> `Unknown
