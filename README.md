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

# References

[The RISC-V Instruction Set Manual](https://riscv.org/wp-content/uploads/2017/05/riscv-spec-v2.2.pdf)
