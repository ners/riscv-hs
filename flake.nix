{
  inputs.nixpkgs.url = github:nixos/nixpkgs/nixos-unstable;
  inputs.flake-utils.url = github:numtide/flake-utils;
  inputs.flake-compat.url = github:edolstra/flake-compat;
  inputs.flake-compat.flake = false;

  outputs = inputs: inputs.flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      mob-overlay = self: super: {
        mob = super.mob.overrideAttrs (old: rec {
          version = "4.0.0";

          src = super.fetchFromGitHub {
            owner = "remotemobprogramming";
            repo = old.pname;
            rev = "v${version}";
            sha256 = "sha256-c6Feex0FGxxOWBEHxe0GqPHv65EwMMdxIeehZUGbl0Q=";
          };
        });
      };
      pkgs = import inputs.nixpkgs { system = system; overlays = [ mob-overlay ]; };
      haskellPackages = pkgs.haskell.packages.ghc92;
    in
    {
      devShells.default = pkgs.mkShell {
        buildInputs = [
          pkgs.mob
          haskellPackages.ghc
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
        ];
      };
    });
}
