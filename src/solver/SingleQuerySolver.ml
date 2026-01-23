(* Solving without spliting the formula to multiple SL queries.
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025 *)

open Context
open Backend_sig
open Translation_sig

module Make (Encoding : ENCODING) (Backend : BACKEND) = struct

  module Translation = Translation.Make(Encoding)(Backend)

  let solve ctx =
    let sl_graph = SL_graph.compute ctx.phi in
    let ctx' = Context.add_metadata ctx sl_graph LocationBounds.empty in
    let ctx'' = Preprocessor.third_phase ctx' in
    let bounds = LocationBounds.compute ctx''.phi ctx''.heap_sort sl_graph in
    Context.add_metadata ctx'' sl_graph bounds
    |> Translation.solve

end
