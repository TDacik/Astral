(* Solver's public API
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module Input = ParserContext

type solver = {
  backend : Config.Backend.t;
  encoding : Config.Encoding.t;
  quantifier_encoding : Config.QuantifierEncoding.t;

  heap_sort : HeapSort.t;

  timeout : int option;
  source : string option;

  (* Options *)
  produce_models : bool;
  use_builtin_defs: bool;
  dump_queries : [`None | `Full of string];

  mutable stats : Unix.process_times list;
}

let reset () =
  PathBound.cache_reset ();
  Profiler.reset ()

let query_id () = LoggerState.current_query ()

let activate solver =
  Config.Interactive.set true;

  let _ = match solver.dump_queries with
    | `None ->
      Config.Debug.set false
    | `Full dir ->
      Config.Debug.set true; Config.DebugDir.set dir
  in

  Config.BackendTimeout.set @@ Option.value ~default:0 solver.timeout;
  Config.ProduceModels.set solver.produce_models;
  Config.Backend.set solver.backend;
  Config.Encoding.set solver.encoding;
  Config.QuantifierEncoding.set solver.quantifier_encoding

let json_stats solver =
  let open Unix in
  let sum t = t.tms_utime +. t.tms_stime +. t.tms_cutime +. t.tms_cstime in
  let total = List.fold_left (fun acc times -> acc +. sum times) 0.0 solver.stats in
  let stats =
    List.mapi (fun i f -> Format.asprintf "Query #%d" i, sum f) solver.stats
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

let init
  ?timeout
  ?(backend=`Bitwuzla)
  ?(encoding=`Bitvectors)
  ?(quantifier_encoding=`Direct)
  ?(produce_models=false)
  ?(use_builtin_defs=true)
  ?(dump_queries=`None)
  ?source
  ()
=
  let solver = {
    backend = backend;
    encoding = encoding;
    quantifier_encoding = quantifier_encoding;

    heap_sort = HeapSort.empty;

    timeout = timeout;
    source = source;

    produce_models = produce_models;
    use_builtin_defs = use_builtin_defs;
    dump_queries = dump_queries;

    stats = [];
  } in
  activate solver;
  Config.check ();
  LoggerState.init ();
  (if solver.use_builtin_defs then begin
    Freed.register ();
    LS.register ();
    DLS.register ();
    NLS.register ()
  end);
  solver

let set_heap_sort heap_sort solver =
  {solver with heap_sort = heap_sort}

let add_heap_sort heap_sort solver =
  {solver with heap_sort = HeapSort.union [heap_sort; solver.heap_sort]}

let add_inductive_definition solver def =
  SID.register_user_defined def

let _solve solver phi =
  reset ();
  activate solver;
  LoggerState.next_query ();
  Profiler.reset ();
  Profiler.add "Start";
  let vars = SL.free_vars ~with_nil:false phi in
  let input =
    let input = if solver.use_builtin_defs then GlobalSID.builtin_context () else Input.empty () in
    let heap_sort = HeapSort.to_list input.heap_sort @ HeapSort.to_list solver.heap_sort in
    let input = Input.add_assertion input phi in
    let input = Input.declare_heap_sort input heap_sort in
    Input.add_vars input vars
  in
  Debug.input "input" ?source:solver.source input;
  let result = Engine.solve input in
  Profiler.finish ();
  Debug.result result;
  solver.stats <- Profiler.total_time () :: solver.stats;
  match Option.get result.status with
  | `Sat -> `Sat (result.model)
  | `Unsat -> `Unsat
  | `Unknown reason -> `Unknown reason

exception UnknownResult of string
exception Timeout

let solve ?timeout solver phi =
  let timeout = match solver.timeout, timeout with
    | None, None -> None
    | Some solver_to, None -> Some solver_to
    | _, Some call_to -> Some call_to
  in
  match timeout with
  | None -> _solve solver phi
  | Some timeout ->
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle (function _ -> raise Timeout));
    ignore @@ Unix.alarm timeout;
    let solver = {solver with timeout = Some timeout} in
    try _solve solver phi
    with Timeout -> `Unknown "astral timeout"

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
