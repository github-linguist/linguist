(lang dune 2.9)

(name reason)

(using menhir 2.0)

(cram enable)

(version 3.8.2)

(generate_opam_files true)

(source
 (github reasonml/reason))

(authors "Jordan Walke <jordojw@gmail.com>")

(maintainers
 "Jordan Walke <jordojw@gmail.com>"
 "Antonio Nuno Monteiro <anmonteiro@gmail.com>")

(homepage "https://reasonml.github.io/")

(bug_reports "https://github.com/reasonml/reason/issues")

(license "MIT")

(package
 (name reason)
 (synopsis "Reason: Syntax & Toolchain for OCaml")
 (description
  "Reason gives OCaml a new syntax that is remniscient of languages like\nJavaScript. It's also the umbrella project for a set of tools for the OCaml &\nJavaScript ecosystem.")
 (depends
  (ocaml
   (and
    (>= "4.03")
    (< "5.2")))
  (ocamlfind :build)
  (dune-build-info
   (>= 2.9.3))
  (menhir
   (>= "20180523"))
  (merlin-extend
   (>= "0.6"))
  fix
  ppx_derivers
  (ppxlib
   (>= "0.28.0"))))

(package
 (name rtop)
 (synopsis "Reason toplevel")
 (description
  "rtop is the toplevel (or REPL) for Reason, based on utop (https://github.com/diml/utop).")
 (depends
  (ocaml
   (and
    (>= "4.03")
    (< "5.2")))
  (reason
   (= :version))
  (utop
   (>= "2.0"))))
