(* Generation of random formulae
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module M1 = Generator.Make
  (struct
    let n_vars = 8
    let depth = 100
    let lists = false
    let unfold = false
  end)

let run n = M1.generate n (Options.quickcheck_store ()) "test"
