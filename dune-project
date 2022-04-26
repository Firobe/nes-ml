(lang dune 3.0)
(name NES-ml)

; (version 1.0.0)

(generate_opam_files true)

(source (github Firobe/NES-ml))
(license MIT)
(authors "Virgile Robles")
(maintainers "virgile.robles@pm.me")

(package
  (name NES-ml)
  (synopsis "Experimental NES emulator")
  (description "Experimental NES emulator")
  (depends
    6502-ml
    tsdl
    extlib
    stdint
    stdint-literals
    (dune :build)))
