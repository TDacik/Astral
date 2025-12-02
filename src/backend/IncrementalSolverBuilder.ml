(*
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

open Backend_sig

module type INCREMENTAL_SOLVER = sig
  include BACKEND
  val push : SMT.t -> unit
  val pop : int -> unit
  val check_sat : SMT.t -> ('term, 'model) status
end

module Make (Solver : INCREMENTAL_SOLVER) = struct

  module Logger = Debug.QueryDir (struct
      let name = "Backend:" ^ Solver.name
      let level = 2
      let dirname = "incremental_queries"
    end)

  (** Query counter *)
  let counter = ref 0
  let next () = incr counter; Format.asprintf "query%0d4.smt2" !counter

  let push = Solver.push
  let pop = Solver.pop

  let check_sat phi =
    let phi' = Solver.translate phi in
    Logger.output (next ()) Solver.show_formula phi';
    let t_start = (Unix.times ()).tms_utime in
    let res = Solver.check_sat phi in
    let t_end = (Unix.times ()).tms_utime in
    Logger.debug "Query %04d: time %f\n" !counter (t_end -. t_start);
    res

end
