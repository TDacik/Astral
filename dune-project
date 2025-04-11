(lang dune 3.17)

(name astral)

(using menhir 2.0)

(wrapped_executables false)

(package
  (name astral)
  (synopsis "Solver for separation logic(s) based on translation to SMT")
  (depends
    (ocaml (>= 5.1.0))
    batteries
    (dolmen (= 0.10))
    ocamlgraph
    sexplib
    yojson
    (ppx_compare :build)
    (ppx_deriving :build)

    (alcotest :with-test)
    (qcheck :with-test)
    (odoc :with-doc)
  )
  (depopts
    bitwuzla
    z3
  )

)

(generate_opam_files)

(license MIT)

(authors
  "Tomáš Dacík <idacik@fit.vut.cz>"
  "Adam Rogalewicz <rogalew@fit.vut.cz>"
  "Tomáš Vojnar <vojnar@fit.vut.cz>"
  "Florian Zuleger<florian.zuleger@tuwien.ac.at>"
)

(maintainers
  "Tomáš Dacík <idacik@fit.vut.cz>"
)

(source (github tdacik/Astral))
(bug_reports https://github.com/TDacik/Astral/issues)
(documentation https://tdacik.github.io/Astral/)
