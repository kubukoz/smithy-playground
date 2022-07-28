{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    smithy4s-nix.url = "github:kubukoz/smithy4s-nix";
    smithy4s-nix.inputs.nixpkgs.follows = "nixpkgs";
    smithy4s-nix.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, smithy4s-nix, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        jvm = final: prev: { jdk = final.openjdk17; jre = final.jdk; };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ jvm smithy4s-nix.overlays.default ];
        };
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.yarn
            pkgs.nodejs-14_x
            pkgs.sbt
            pkgs.smithy4s.codegen
            pkgs.jless
            pkgs.gnupg
          ];
        };
      }
    );
}
