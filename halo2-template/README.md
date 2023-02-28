# Halo 2 example (with Nix)

## Usage

To get a dev shell:

```
nix develop
```

Starting a dev shell should output the following usage instructions:

```
Commands:
  gen_proof a b    a and b are numbers and the proof is written
                   to ./proof
  verify c         c = a * b and the proof is read from ./proof
  help             show this message
```
