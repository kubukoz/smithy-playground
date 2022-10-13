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
          buildInputs = [ pkgs.tree-sitter pkgs.nodejs ];
          buildPhase = ''
            tree-sitter generate
            cc src/parser.c -o parser.so -Isrc -shared
          '';
          installPhase = ''
            cp parser.so $out
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
        packages.grammar-all = pkgs.linkFarm "grammar-all" [
          {
            name = "darwin-x86_64/libtree-sitter-smithyql.dylib";
            path = self.packages.x86_64-darwin.grammar;
          }
          {
            name = "darwin-aarch64/libtree-sitter-smithyql.dylib";
            path = self.packages.aarch64-darwin.grammar;
          }
          {
            name = "linux-x86_64/libtree-sitter-smithyql.so";
            path = self.packages.x86_64-linux.grammar;
          }
          {
            name = "linux-aarch64/libtree-sitter-smithyql.so";
            path = self.packages.aarch64-linux.grammar;
          }

        ];
      }
    );
}
