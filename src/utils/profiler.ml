(* Utility for profiling.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Unix

let tms_zero = {
  tms_utime = 0.0;
  tms_stime = 0.0;
  tms_cutime = 0.0;
  tms_cstime = 0.0;
}

let (--) times1 times2 = {
  tms_utime = times1.tms_utime -. times2.tms_utime;
  tms_stime = times1.tms_stime -. times2.tms_stime;
  tms_cutime = times1.tms_cutime -. times2.tms_cutime;
  tms_cstime = times1.tms_cstime -. times2.tms_cstime;
}

let start = ref tms_zero

let stop = ref tms_zero

let self : (string * Unix.process_times) list ref = ref []

let reset () =
  start := Unix.times ();
  self := []

let add name =
  self := ((name, Unix.times ()) :: !self)

let finish () = stop := Unix.times ()

let compute_stats () =
  let _, stats = List.fold_left
    (fun (prev, acc) (name, current) ->
      (current, (name, current, (current -- prev)) :: acc)
    ) (tms_zero, []) (List.rev !self)
  in
  List.rev stats

let total_time () = !stop -- !start

let report () =
  let stats = compute_stats () in
  Format.printf "[Profiler]\n\n";
  Format.print_flush ();
  Format.open_tbox ();
  Format.printf "  ";
  Format.set_tab (); Format.printf "Phase                ";
  Format.set_tab (); Format.printf "Total                ";
  Format.set_tab (); Format.printf "Diff                 ";
  Format.set_tab (); Format.printf "Total (childs)  ";
  Format.set_tab (); Format.printf "Diff (childs)";
  Format.printf "\n  --------------------------------------------------";

  List.iter
    (fun (name, time, diff) ->
      Format.print_tbreak 0 0;
      Format.printf "%s" name;
      Format.print_tbreak 0 0;
      Format.printf "%f" (time.tms_utime +. time.tms_stime);
      Format.print_tbreak 0 0;
      Format.printf "%f" (diff.tms_utime +. diff.tms_stime);
      Format.print_tbreak 0 0;
      Format.printf "%f" (time.tms_cutime +. time.tms_cstime);
      Format.print_tbreak 0 0;
      Format.printf "%f" (diff.tms_cutime +. diff.tms_cstime);
    ) stats;

  Format.close_tbox ();
  Format.printf "\n\n"

(** Json output *)

let json_total () =
  let total = !stop -- !start in
  `Assoc [
    "User time",   `Float total.tms_utime;
    "System time", `Float total.tms_stime;
    "User time (childs)",   `Float total.tms_cutime;
    "System time (cstime)", `Float total.tms_cstime;
  ]

let json_phases () =
  let stats = compute_stats () in
  `List (List.map (fun (name, time, diff) ->
    `Assoc [ name, `Assoc [
      "Diff", `Float (diff.tms_utime +. diff.tms_stime);
      "Diff (childs)", `Float (diff.tms_cutime +. diff.tms_cstime)
    ]]
  ) (compute_stats ()))

let json_repr () =
  let stats = compute_stats () in
  `Assoc [
    "Total time", json_total ();
    "Phases",     json_phases ();
  ]
