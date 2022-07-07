(* Utilities for performance measurement
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Unix

let self = ref []

let total = ref Float.minus_one

let add name = self := ((name, (Unix.times ()).tms_utime) :: !self)

let finish () = total := (Unix.times ()).tms_utime

let compute_stats () =
  List.fold_left
    (fun (prev, acc) (name, t) ->
      (t, (name, t, t -. prev) :: acc)
    ) (0.0, []) (List.rev !self)
  |> snd
  |> List.rev


let json_repr () =
  `Assoc (List.map (fun (name, t) -> (name, `Float t)) !self)

let report () =
  if Options.profile () then begin
    let stats = compute_stats () in
    Format.open_tbox ();
    Format.printf "[Profiler]";
    Format.print_tbreak 0 2;
    Format.set_tab ();
    Format.printf "total";
    Format.print_tbreak 0 2;
    Format.set_tab ();
    Format.printf "difference";
    List.iter
      (fun (name, time, diff) ->
        Format.printf "%s" name;
        Format.print_tbreak 0 2;
        Format.print_tab ();
        Format.printf "%f" time;
        Format.print_tbreak 0 2;
        Format.print_tab ();
        Format.printf "%f" diff;
        Format.print_tbreak 0 2;
      ) stats;
    Format.close_tbox ();
    Format.printf "\n"
  end
