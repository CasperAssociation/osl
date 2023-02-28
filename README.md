# Open Specification Language (OSL)

This repo contains a compiler for the OSL spec language, which targets arithmetic circuits for zero knowledge proofs.

OSL (Open Specification Language) is a fork, by Casper Association, of OSL (Orbis Specification Language), by Orbis Labs.

For information about OSL, see: https://eprint.iacr.org/2022/1003

This version of OSL differs from the one described in the above paper in that it features bounded quantification, as in: https://eprint.iacr.org/2022/1105

## System requirements

The system must have Nix, with flakes enabled.

To install Nix: https://nixos.org/download.html

To enable flakes: https://nixos.wiki/wiki/Flakes#Enable_flakes

## Building

To build the OSL compiler, run:

```
nix build
```

If successful, the binary will be output at `./result/bin/osl`.

## Usage

The compiler expects two command line arguments: first, a path to an OSL source file, and second, the name of a declaration in the file. The provided name must name a value of type

```
a1 -> ... -> an -> Prop
```

for some types `a1, ..., an`.

Example usage:

```
./result/bin/osl examples/sudoku.osl problemIsSolvable
```

## Developing

To enter the dev shell, run:

```
nix develop
```

To get interactive compiler feedback, run:

```
ghcid --command "cabal repl"
```

To run the tests:

```
cabal test
```

To run the formatter and linter:

```
./lint.sh
```

You need to run this script on a clean checkout (i.e. with all changes
committed to git). If the formatter makes changes, then commit those
changes, and re-run the script to see if there are any lint hints.
