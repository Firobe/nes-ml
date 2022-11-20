(lang dune 3.0)
(name nes-ml)

; (version 1.0.0)

(generate_opam_files true)

(source (github Firobe/NES-ml))
(license MIT)
(authors "Virgile Robles")
(maintainers "virgile.robles@pm.me")

(package
  (name nes-ml)
  (synopsis "Experimental NES emulator")
  (description "Experimental NES emulator")
  (depends
    6502-ml
    tsdl
    stdint
    stdint-literals
    fpath
    bos
    digestif
    cmdliner
    bogue
    (dune :build)))
