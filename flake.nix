{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.yarn
            pkgs.nodejs
            pkgs.sbt
            pkgs.jless
            pkgs.gnupg
            # temporary, while we don't download coursier ourselves
            pkgs.coursier
          ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.xvfb-run ];
        };
      }
    );
}
