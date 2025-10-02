(*
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

open Backend_sig

module type OPTIMIZING_SOLVER = sig
  include BACKEND
  val maximize : objective:SMT.t -> SMT.t -> ('term, 'model) status
  val minimize : objective:SMT.t -> SMT.t -> ('term, 'model) status
end

module Make (Solver : OPTIMIZING_SOLVER) = struct

  module Logger = Logger.Make(struct
      let name = "Backend:" ^ Solver.name
      let level = 2
    end)

  let maximize = Solver.maximize
  let minimize = Solver.minimize

end
