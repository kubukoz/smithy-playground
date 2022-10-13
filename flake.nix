{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        jvm = final: prev: { jdk = final.openjdk17; jre = final.jdk; };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ jvm ];
        };
        grammar = pkgs.stdenv.mkDerivation {
          pname = "tree-sitter-smithyql";
          version = "0.0.0";
          src = ./tree-sitter-smithyql;
          buildPhase = ''
            cc src/parser.c -o parser.so -Isrc -shared
          '';
          installPhase = ''
            mkdir -p $out
            cp parser.so $out/parser
          '';
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.yarn
            pkgs.nodejs-14_x
            pkgs.sbt
            pkgs.jless
            pkgs.gnupg
            pkgs.tree-sitter
          ];
        };
        packages.grammar = grammar;
      }
    );
}
