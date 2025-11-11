(* Mutable representation of commandline parameters and conversion to Arg.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

type 'a value = {
  set : 'a -> unit;
  get : unit -> 'a;
}

type kind =
  | Bool of bool ref * bool
  | Int of int value
  | String of string value
  | Action of (unit -> unit)

type param = {
  kind : kind;
  name : string;
  short_name : char option;
  help : string option;
}

(** List of registered parameters *)
let params = ref []

let register ?short_name ?help name kind =
  params := {kind; name; short_name; help = help} :: !params

(** Init *)

let indent_threshold () =
  List.map (fun p -> String.length p.name) !params
  |> List.fold_left max 0
  |> min 22
  |> (+) 2

let print_help_and_exit () =
  let treshold = indent_threshold () in
  List.iter (fun p ->
    match p.help with
    | None -> ()
    | Some help ->
      if String.length p.name >= treshold then
        let _ = Format.printf "%s@," p.name in
        Format.printf "  %*s@[<v>%a@]@." treshold "" Format.pp_print_text help
      else
        Format.printf "  %-*s@[<hov 0>%a@]@." treshold (p.name ^ "  ") Format.pp_print_text help
  ) (List.rev !params);
  exit 0

let () =
  register "--help" ~short_name:'h' ~help:"Display this list of options" (Action print_help_and_exit)

let convert_to_args params =
  let negate opt = "--no-" ^ Str.string_after opt 2 in
  List.fold_left (fun acc param ->
    let help = Option.value ~default:"" param.help in
    let args = match param.kind with
      | Bool (ref, _) -> [
          (param.name, Arg.Set ref, help);
          (negate param.name, Arg.Clear ref, "")
        ]
      | Int {set; _} -> [(param.name, Arg.Int set, help)]
      | String {set; _} -> [(param.name, Arg.String set, help)]
      | Action fn -> [(param.name, Arg.Unit fn, help)]
    in
    let short_args = match param.short_name, param.kind with
      | None, _ -> []
      | Some c, Bool (ref, _) -> ["-" ^ Char.escaped c, Arg.Set ref, ""]
      | Some c, Int {set; _} -> ["-" ^ Char.escaped c, Arg.Int set, ""]
      | Some c, String {set; _} -> ["-" ^ Char.escaped c, Arg.String set, ""]
      | Some c, Action fn -> ["-" ^ Char.escaped c, Arg.Unit fn, ""]
    in
    short_args @ args @ acc
  ) [] params

let parse ?version handle_input usage_msg =
  Arg.parse (convert_to_args !params) handle_input usage_msg

let print () =
  List.iter (fun param ->
    match param.kind with
      | Bool (ref, _) -> Format.printf "%s: %b\n" param.name (!ref)
      | Int {get; _} -> Format.printf "%s: %d\n" param.name (get ())
      | String {get; _} -> Format.printf "%s: %s\n" param.name (get ())
      | _ -> ()
  ) !params

let to_json () =
  `Assoc (
    List.filter_map (fun param -> match param.kind with
      | Bool (ref, _) -> Some (param.name, `Bool !ref)
      | Int {get; _} -> Some (param.name, `Int (get ()))
      | String {get; _} -> Some (param.name, `String (get ()))
      | Action _ -> None
    ) !params
  )
