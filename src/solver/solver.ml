(* Solver's public API
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module Input = ParserContext

type solver = {
  backend : Options.backend;
  encoding : Options.encoding;

  (* Options *)
  produce_models : bool;
  use_builtin_defs: bool;
  dump_queries : [`None | `Full of string];

  mutable stats : Float.t list;
}

let reset () =
  PathBound.cache_reset ();
  Profiler.reset ()

let activate solver =
  Debug.next_query ();
  Options_base.set_interactive true;

  let _ = match solver.dump_queries with
    | `None -> Options_base.set_debug false
    | `Full dir -> Options_base.set_debug true; Options_base.set_debug_dir dir
  in

  (* TODO: maybe elsewhere? *)
  (if solver.use_builtin_defs then begin
    LS.register (); DLS.register (); NLS.register ()
  end);

  Options.set_produce_models solver.produce_models;
  Options.set_backend solver.backend;
  Options.set_encoding solver.encoding

let json_stats solver =
  let total = BatList.fsum solver.stats in
  let stats =
    List.mapi (fun i f -> Format.asprintf "Query #%d" i, f) solver.stats
    |> List.sort (fun (_, f1) (_, f2) -> Float.compare f1 f2)
    |> List.rev
  in
  `Assoc [
     "Total time", `Float total;
     "Queries",    `Assoc (List.map (fun (name, f) -> name, `Float f) stats)
   ]

let dump_stats solver = match solver.dump_queries with
  | `None -> ()
  | `Full dir ->
    let path = Filename.concat dir "summary.json" in
    let channel = open_out_gen [Open_creat; Open_wronly] 0o666 path in
    Yojson.Basic.pretty_to_channel channel @@ json_stats solver;
    close_out channel

let init ?(backend=`Z3) ?(encoding=`Sets) ?(produce_models=false) ?(use_builtin_defs=true) ?(dump_queries=`None) () =
  let solver = {
    backend = backend;
    encoding = encoding;

    produce_models = produce_models;
    use_builtin_defs = use_builtin_defs;
    dump_queries = dump_queries;

    stats = [];
  } in
  activate solver;
  Options.check ();
  Debug.init ();
  solver

let solve solver phi =
  reset ();
  activate solver;
  Profiler.add "Start";
  let vars = SL.free_vars phi in
  let input =
    let input = if solver.use_builtin_defs then SID.builtin_context () else Input.empty () in
    let input = Input.add_assertion input phi in
    Input.add_vars input vars
  in
  let result = Engine.solve input in
  Profiler.finish ();
  Debug.result result;
  solver.stats <- Profiler.total_time () :: solver.stats;
  match Option.get result.status with
  | `Sat -> `Sat (result.model)
  | `Unsat -> `Unsat
  | `Unknown reason -> `Unknown reason

exception UnknownResult of string

let lift res = function
  | `Sat _ -> res
  | `Unsat -> not res
  | `Unknown reason -> raise @@ UnknownResult reason

let check_sat solver phi = lift true @@ solve solver phi

let check_entl solver lhs rhs =
  let phi = SL.mk_gneg lhs rhs in
  lift false @@ solve solver phi

let check_equiv solver lhs rhs =
  let phi1 = SL.mk_gneg lhs rhs in
  let phi2 = SL.mk_gneg rhs lhs in
  let phi = SL.mk_or [phi1; phi2] in
  lift false @@ solve solver phi
