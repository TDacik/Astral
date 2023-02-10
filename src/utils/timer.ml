(* Utilities for performance measurement
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Unix

let self : (string * float * float) list ref = ref []

let total = ref Float.minus_one

let add name =
  let times = Unix.times () in
  self := ((name, times.tms_utime, times.tms_cutime) :: !self)

let finish () = total := (Unix.times ()).tms_utime

let compute_stats () =
  let _, _, stats = List.fold_left
    (fun (prev1, prev2, acc) (name, t1, t2) ->
      (t1, t2, (name, t1, t1 -. prev1, t2, t2 -. prev2) :: acc)
    ) (0.0, 0.0, []) (List.rev !self)
  in
  List.rev stats


let json_repr () =
  `Assoc (List.map (fun (name, t1, t2) -> (name, `List [`Float t1; `Float t2])) !self)

let report () =
  if Options.profile () then begin
    let stats = compute_stats () in
    Format.printf "[Profiler]\n\n";
    Format.print_flush ();
    Format.open_tbox ();
    Format.printf "  ";
    Format.set_tab (); Format.printf "Phase                ";
    Format.set_tab (); Format.printf "Total                ";
    Format.set_tab (); Format.printf "Total (childs)       ";
    Format.set_tab (); Format.printf "Diff                 ";
    Format.set_tab (); Format.printf "Diff (childs)";
    Format.printf "\n  --------------------------------------------------";

    List.iter
      (fun (name, time, diff, subprocess_time, subprocess_diff) ->
        Format.print_tbreak 0 0;
        Format.printf "%s" name;
        Format.print_tbreak 0 0;
        Format.printf "%f" time;
        Format.print_tbreak 0 0;
        Format.printf "%f" subprocess_time;
        Format.print_tbreak 0 0;
        Format.printf "%f" diff;
        Format.print_tbreak 0 0;
        Format.printf "%f" subprocess_diff;
      ) stats;
    Format.close_tbox ();
    Format.printf "\n\n";
  end
