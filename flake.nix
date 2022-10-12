{
  inputs.nixpkgs.url = github:nixos/nixpkgs/nixos-unstable;
  inputs.flake-utils.url = github:numtide/flake-utils;

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import inputs.nixpkgs { system = system; };
    in
    {
      devShells.default = pkgs.mkShell {
        buildInputs = [ pkgs.fortune pkgs.cowsay pkgs.lolcat ];
      };
    });
}
