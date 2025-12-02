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
    let bounds = LocationBounds.compute ctx.phi ctx.raw_input.heap_sort sl_graph in
    Context.add_metadata ctx sl_graph bounds
    |> Preprocessor.third_phase
    |> Translation.solve

end
