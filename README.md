# NES-ml

Needs two unreleased libraries to be installed:
- separate CPU library: [`6502-ml`](https://github.com/Firobe/6502-ml)
- fixed-size int literals: [`stdint-literals`](https://github.com/Firobe/ocaml-stdint-literals)

## Build & Install

Run `opam install .` or just `dune build`. The executable name is simply `nes`.

## Usage

Use : `nes PATH_TO_ROM`
Hard-coded controls :
- A : s
- B : d
- Arrows : arrows
- Start : return
- Select : backspace
- Home: open debugging windows
- End: close debugging windows
