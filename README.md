# nes-ml

Based on two unreleased libraries:
- separate CPU library: [`6502-ml`](https://github.com/Firobe/6502-ml)
- fixed-size int literals: [`stdint-literals`](https://github.com/Firobe/ocaml-stdint-literals)

## Build & Install

Run `opam install .`. This will fetch the unreleased libraries, build and
install the emulator.

The executable name is `nes-ml`.
I recommend using a build with `flambda` activated for more performance.

## Usage

Use : `nes PATH_TO_ROM`  

### Controls (hard-coded)

| Function | Keyboard key |
| --- | --- |
| A button | s |
| B button | d |
| Left arrow | ← |
| Right arrow | → |
| Up arrow | ↑ |
| Down arrow | ↓ |
| Start | ⏎ (return) |
| Select | ⌫ (backspace) |
| Save state in slot `N` | `N` |
| Load state from slot `N` | Shift+`N` |
| Toggle debugging windows | Home |

Note that the `N` for save states must be 1, 2 or 3.

## Development status

- Barebones GUI (with debugging windows showing the internal PPU state)
- **Cycle-accute CPU** (see [`6502-ml`](https://github.com/Firobe/6502-ml))
- **Cycle-accurate PPU** (graphics) (with rough edges)
- Partially implemented cycle-accurate **APU** (sound) (with rough edges)
- Multiple **save states**
- Implemented mappers: 0, 2
