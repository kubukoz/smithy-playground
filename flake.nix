{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (final: prev:
              let
                jre = final.openjdk11;
                jdk = jre;
              in
              { inherit jdk jre; })
          ];
        };
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.yarn
            pkgs.nodejs-14_x
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
