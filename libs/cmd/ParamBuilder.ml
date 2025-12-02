(* Functors for building parameters.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

open Param_sig

exception OptionError of string

let raise_value_error ~name ~value ~expected =
  let msg =
    Format.asprintf "Option %s got invalid value %s (expected %s)"
      name value expected
  in
  raise @@ OptionError msg

(** Parameter which is initially set. *)
module True (Param : PARAM) = struct
  include Param

  let value = ref true
  let default = true
  let set flag = value := flag
  let get () = !value

  let () = CommandLine.register ~help ?short_name name (Bool (value, true))
end

module Int (Param : INT_IN) = struct
  include Param

  let value = ref default
  let default = default
  let min = min
  let max = max

  let set n =
    if n >= min && n <= max then value := n
    else
      raise_value_error
        ~name
        ~value:(string_of_int n)
        ~expected:(Format.asprintf "value in range [%d, %d]" min max)

  let get () = !value

  let () = CommandLine.register ~help ?short_name name (Int {set; get})

end

module PositiveInt (Param : VALUE_IN with type t := int) = struct
  include Int(struct
    include Param
    let min = 0
    let max = Stdlib.Int.max_int
  end)
end

(** Parameter which is not initially set. *)
module False (Param : PARAM) = struct
  include Param

  let value = ref false
  let default = false
  let set flag = value := flag
  let get () = !value

  let () = CommandLine.register ~help ?short_name name (Bool (value, false))
end

module String (Param : VALUE_IN with type t := string) = struct
  include Param

  let value = ref default
  let set x = value := x
  let get () = !value

  let () = CommandLine.register ~help ?short_name name (String {set; get})
end

module Path = String (* TODO *)

(** Enums *)

module Enum (E : ENUM_IN) = struct
  module String = Stdlib.String
  include E

  (** Redefine show. *)
  let show value =
    let default =
      show value
      |> String.lowercase_ascii
      |> String.map (function '_' -> '-' | c -> c)
    in
    match default.[0] with
    | '`' -> Str.string_after default 1
    | _ -> default

  let value = ref default

  let values =
    let x = max - min + 1 in
    List.init x (fun x -> Option.get @@ of_enum (x + min))

  let names = List.map show values

  let of_string str =
    match List.find_opt (fun v -> String.equal (show v) str) values with
    | Some v -> v
    | None -> raise_value_error ~name ~value:str ~expected:(String.concat ", " names)

  let set const = value := const
  let get () = !value

  let set_string str = set @@ of_string str
  let get_string () = show @@ get ()


  let () = CommandLine.register ~help ?short_name name (String {set = set_string; get = get_string})

end

module Action (A : ACTION) = struct
  include A
  let () = CommandLine.register ~help ?short_name name (Action action)
end
