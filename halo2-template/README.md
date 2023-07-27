# OSL Halo 2 output template

To get a dev shell:

```
nix develop
```

To update the `Cargo.nix` and `flake.lock` files after changing `Cargo.toml`:

```
 $ nix develop
 [nix-develop-osl-output:] cargo build
 [nix-develop-osl-output:] exit
 $ nix run github:cargo2nix/cargo2nix
```

Running `cargo build` ensures that the `Cargo.lock` file is up to date before running `cargo2nix`.
