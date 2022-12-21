# RISCV-HS

A logical model of a minimal RISC-V CPU written in Haskell.

This is an exercise to learn Nix, Haskell, and RISC-V.
It is not intended to be useful, fast, or even complete.

The goal is to understand the inner workings of the RISC-V architecture from the bottom-up and learn Haskell concepts on the way.
A possible outcome would be an emulator that can run RV32I assembly.

# Requirements

- Install Nix
  - follow instructions on https://nixos.org/download
- Enable flakes
  - follow instructions on https://nixos.wiki/wiki/Flakes#Enable_flakes
- (Optional) install [`direnv`](https://direnv.net)

# Project structure
- The structure of the project is dictated by the tools we are using, namely Nix and Cabal. This repository is both a Nix flake and a Cabal package and as such contains the following folders/files:
  - [`app/`](/app): Haskell source code for an executable that can be built and run by `cabal run`.
  - [`src/`](/src): Haskell library folder, each file inside is a Haskell module.
  - [`test/`](/test): Haskell source code for an executable that is built and run by `cabal test`, used to test the functions defined in `src/`.
  - [`CHANGELOG.md`](CHANGELOG.md): This file will only become relevant in case of a release.
  - [`flake.nix`](flake.nix): This is a [Nix flake](https://nixos.wiki/wiki/Flakes) providing dependencies for the project.
  - [`flake.lock`](flake.lock): File specifying the exact versions of dependencies, created by the [`nix flake lock`](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake-lock.html) command. 
  - [`LICENSE`](LICENSE): The Apache 2.0 license.
  - [`riscvs-hs.cabal`](riscvs-hs.cabal): Cabal [package description](https://cabal.readthedocs.io/en/3.4/cabal-package.html).
  - [`shell.nix`](shell.nix): For compatibility with Nix without flakes as well as other tools.

# Notes
- If you're using VSCode, you can use [Nix environment selector](https://github.com/arrterian/nix-env-selector) to connect it with the Nix shell.

# References

[The RISC-V Instruction Set Manual](https://riscv.org/wp-content/uploads/2017/05/riscv-spec-v2.2.pdf)
