{
  inputs.nixpkgs.url = github:nixos/nixpkgs/nixos-unstable;
  inputs.flake-utils.url = github:numtide/flake-utils;
  inputs.flake-compat.url = github:edolstra/flake-compat;
  inputs.flake-compat.flake = false;

  outputs = inputs: inputs.flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      pkgs = import inputs.nixpkgs { system = system; overlays = [ ]; };
      haskellPackages = pkgs.haskell.packages.ghc92.override {
        overrides = self: super: {
          riscv-hs = self.callCabal2nix "risv-hs" ./. { };
        };
      };
      haskellDeps = drv: with builtins; concatLists (attrValues drv.getCabalDeps);
    in
    {
      packages = pkgs // {
        inherit haskellPackages;
      };
      devShells.default = pkgs.mkShell {
        buildInputs = [
          (haskellPackages.ghcWithPackages (ps: haskellDeps ps.riscv-hs))
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
        ];
      };
    }
  );
}
