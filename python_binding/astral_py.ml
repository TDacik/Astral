(* Python binding
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Astral_lib

open Base
open Python_lib
open Python_lib.Let_syntax

open Python_types
open Solver_api

let f = Defunc.Of_python.create ~type_name:"formula" ~conv:formula_of_python

let v = Defunc.Of_python.create ~type_name:"variable" ~conv:variable_of_python

let vars =
  Defunc.Of_python.create ~type_name:"variable list" ~conv:(list_of_python variable_of_python)

let ssl_var =
  let%map_open x = positional "name" string ~docstring:"name" in
  (fun () -> python_of_variable (SSL.Variable.mk x))

let ssl_nil = Defunc.no_arg (fun () () -> python_of_variable SSL.Variable.Nil)

(* ==== Formula constructors ==== *)

let ssl_emp = Defunc.no_arg (fun () () -> python_of_formula @@ SSL.mk_emp ())

(** Generic constructor for atomic predicates *)
let gen_ssl_atom fn =
  let%map_open x = positional "x" v ~docstring:"x"
           and y = positional "y" v ~docstring:"y" in
  (fun () -> python_of_formula (fn x y))

let gen_ssl_unary fn =
  let%map_open phi = positional "phi" f ~docstring:"phi" in
  (fun () -> python_of_formula (fn phi))

let gen_ssl_binary fn =
  let%map_open lhs = positional "lhs" f ~docstring:"lhs"
           and rhs = positional "rhs" f ~docstring:"rhs" in
  (fun () -> python_of_formula (fn lhs rhs))

let gen_ssl_binary_list fn =
  let%map_open lhs = positional "lhs" f ~docstring:"lhs"
           and rhs = positional "rhs" f ~docstring:"rhs" in
  (fun () -> python_of_formula (fn [lhs; rhs]))

let ssl_eq   = gen_ssl_atom SSL.mk_eq
let ssl_neq  = gen_ssl_atom SSL.mk_neq
let ssl_pto  = gen_ssl_atom SSL.mk_pto
let ssl_ls   = gen_ssl_atom SSL.mk_ls

let ssl_not  = gen_ssl_unary SSL.mk_not
let ssl_and  = gen_ssl_binary_list SSL.mk_and
let ssl_or   = gen_ssl_binary_list SSL.mk_or

let ssl_star = gen_ssl_binary_list SSL.mk_star
let ssl_septraction = gen_ssl_binary SSL.mk_septraction
let ssl_wand        = gen_ssl_binary SSL.mk_wand

let solve =
  let%map_open phi = positional "phi" f ~docstring:"input formula"
          and vars = positional "vars" vars ~docstring:"variables" in
  (fun () -> python_of_result (solve phi vars))

let () =
  if not @@ Py.is_initialized () then Py.initialize ();

  (* Constructors for SSL formulae *)
  let ssl = Py_module.create "SSL" in

  Py_module.set ssl "Nil"   ssl_nil;
  Py_module.set ssl "Var"   ssl_var;

  Py_module.set ssl "Eq"    ssl_eq;
  Py_module.set ssl "Neq"   ssl_neq;
  Py_module.set ssl "Emp"   ssl_emp;
  Py_module.set ssl "Pto"   ssl_pto;
  Py_module.set ssl "List"  ssl_ls;

  Py_module.set ssl "Not"   ssl_not;
  Py_module.set ssl "And"   ssl_and;
  Py_module.set ssl "Or"    ssl_or;

  Py_module.set ssl "Star"  ssl_star;
  Py_module.set ssl "Septraction"  ssl_septraction;
  Py_module.set ssl "Wand"  ssl_wand;

  (* Solver *)
  let solver = Py_module.create "solver" in
  Py_module.set solver "solve" solve
