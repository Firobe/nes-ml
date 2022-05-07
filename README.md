# NES-ml

Needs two unreleased libraries to be installed:
- separate CPU library: [`6502-ml`](https://github.com/Firobe/6502-ml)
- fixed-size int literals: [`stdint-literals`](https://github.com/Firobe/ocaml-stdint-literals)

## Build & Install

Run `opam install .` or just `dune build`. The executable name is simply `nes`.
I recommend using a build with `flambda` activated for more performance.

## Usage

Use : `nes PATH_TO_ROM`
Hard-coded controls :
- A : s
- B : d
- Arrows : arrows
- Start : return
- Select : backspace
- Toggle debugging windows: home
- Save state in slot 1/2/3: 1/2/3
- Load state from slot 1/2/3: Shift+1/2/3

## Development status

- Barebones GUI (with debugging windows showing the internal PPU state)
- Cycle-accute CPU (see [`6502-ml`](https://github.com/Firobe/6502-ml))
- Cycle-accurate PPU (with rough edges)
- Partially implemented cycle-accurate APU (with rough edges)
- Multiplke save states
- Implemented mappers: 0, 2
