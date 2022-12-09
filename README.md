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
  - `(./app/)`: Contains Haskell source for an executable that can be built and run by `cabal run`. It does not do anything meaningful at the moment.
  - `(./src/)`: Haskell library folder, each file inside is a Haskell module providing functions-
  - `(./test/)` : Contains Haskell source for an executable that is built and run by `cabal test`, used to test the functions defined in `src/`.
  - `(./CHANGELOG.md)`: This file will only become relevant in case of a release.
  - `(./flake.nix)` : This is a [Nix flake](https://nixos.wiki/wiki/Flakes) providing dependencies for the project, namely GHC and Cabal, for Linux and MacOS/Darwin on x86_64.
  - `(./flake.lock)`: File automatically created by Nix that does not need to be touched.
  - `(./LICENSE)`: The Apache 2.0 license.
  - `(./riscvs-hs.cabal)`: Cabal [package description](https://cabal.readthedocs.io/en/3.4/cabal-package.html).
  - `(./shell.nix)`: For compatibility with non-flake-enabled Nix versions.
# References

[The RISC-V Instruction Set Manual](https://riscv.org/wp-content/uploads/2017/05/riscv-spec-v2.2.pdf)
