external py_init : unit -> unit = "PyInit_astral_py"

let () =
  Printf.printf "toplevel -- init\n";
  ignore (py_init)
