external py_init : unit -> unit = "PyInit_astral_py"

let () = ignore (py_init)
