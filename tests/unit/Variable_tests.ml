module MakeVar () = struct
  module Var = struct
    include Variable.Make ()
    let mk name = mk name Sort.int
    let mk_fresh name = mk_fresh name Sort.int
    let show = show_debug
  end
  include Var
  include Builder.Make(Var)
end

let mk_test1 () =
  let module Var = MakeVar () in
  let x = Var.mk "x" in
  let y = Var.mk "y" in
  Var.check_distinct x y

let mk_test2 () =
  let module Var = MakeVar () in
  let x1 = Var.mk "x" in
  let x2 = Var.mk "x" in
  Var.check_equal x1 x2

let mk_fresh_test1 () =
  let module Var = MakeVar () in
  let x = Var.mk "x" in
  let x' = Var.mk_fresh "x" in
  Var.check_distinct x x'

let mk_fresh_test2 () =
  let module Var = MakeVar () in
  let x1 = Var.mk_fresh "x" in
  let x2 = Var.mk_fresh "x" in
  Var.check_distinct x1 x2

let mk_fresh_test3 () =
  let module Var = MakeVar () in
  let x1 = Var.mk "x" in
  let _  = Var.mk_fresh "x" in
  let x2 = Var.mk "x" in
  Var.check_equal x1 x2

let mk_fresh_test4 () =
  let module Var = MakeVar () in
  let x1 = Var.mk_fresh "x" in (* name x!1 *)
  let x2 = Var.of_description @@ Var.describe x1 in
  Var.check_equal ~msg:(Var.debug()) x1 x2

let refresh_test1 () =
  let module Var = MakeVar () in
  let x1 = Var.mk "x" in
  let x2 = Var.refresh x1 in
  Var.check_distinct x1 x2

let refresh_test2 () =
  let module Var = MakeVar () in
  let x1 = Var.mk_fresh "x" in
  let x2 = Var.refresh x1 in
  Var.check_distinct x1 x2

let refresh_test3 () =
  let module Var = MakeVar () in
  let x = Var.mk_fresh "x" in
  let x1 = Var.refresh x in
  let x2 = Var.refresh x in
  Var.check_distinct x1 x2

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
      test_case "Reusing fresh variable" `Quick mk_fresh_test4;
    ];
    "refresh", [
      test_case "mk(x) != refresh(x)" `Quick refresh_test1;
      test_case "mk_fresh(x) != refresh(x)" `Quick refresh_test2;
      test_case "refresh(x) != refresh(x)" `Quick refresh_test3;
    ];
  ]
