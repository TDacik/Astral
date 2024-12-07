module MakeVar () = struct
  include Variable.Make ()

  let mk name = mk name Sort.Int
  let mk_fresh name = mk_fresh name Sort.Int
end

let mk_test1 () =
  let module Var = MakeVar () in
  let x = Var.mk "x" in
  let y = Var.mk "y" in
  assert (not @@ Var.equal x y)

let mk_test2 () =
  let module Var = MakeVar () in
  let x1 = Var.mk "x" in
  let x2 = Var.mk "x" in
  assert (Var.equal x1 x2)

let mk_fresh_test1 () =
  let module Var = MakeVar () in
  let x = Var.mk "x" in
  let x' = Var.mk_fresh "x" in
  assert (not @@ Var.equal x x')

let mk_fresh_test2 () =
  let module Var = MakeVar () in
  let x1 = Var.mk_fresh "x" in
  let x2 = Var.mk_fresh "x" in
  assert (not @@ Var.equal x1 x2)

let mk_fresh_test3 () =
  let module Var = MakeVar () in
  let x1 = Var.mk "x" in
  let _  = Var.mk_fresh "x" in
  let x2 = Var.mk "x" in
  assert (Var.equal x1 x2)


let () =
  run "Variables" [
    "mk", [
      test_case "mk(x) != mk(y)" `Quick mk_test1;
      test_case "mk(x) == mk(x)" `Quick mk_test2;
    ];
    "mk_fresh", [
      test_case "mk_fresh(x) != mk(x)" `Quick mk_fresh_test1;
      test_case "mk_fresh(x) != mk_fresh(x)" `Quick mk_fresh_test2;
      test_case "Fresh/used sequence" `Quick mk_fresh_test3;
    ];
  ]
