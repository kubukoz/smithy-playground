{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    treesitter4s.url = "github:polyvariant/treesitter4s/native";
    treesitter4s.inputs.nixpkgs.follows = "nixpkgs";
    treesitter4s.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, treesitter4s, ... }@inputs:
    let
      ts-lib = treesitter4s.lib; in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        jvm = final: prev: { jdk = final.openjdk17; jre = final.jdk; };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ jvm ];
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
        packages.grammar =
          let base = pkgs.stdenv.mkDerivation {
            pname = "tree-sitter-smithyql-generated";
            version = "0.0.0";
            src = ./tree-sitter-smithyql;
            buildInputs = [ pkgs.tree-sitter pkgs.nodejs ];
            buildPhase = ''
              tree-sitter generate
            '';
            installPhase = ''
              cp -r . $out
            '';
          }; in
          pkgs.callPackage "${nixpkgs}/pkgs/development/tools/parsing/tree-sitter/grammar.nix" { } {
            language = "tree-sitter-smithyql";
            inherit (base) version;
            source = base;
          };
        packages.grammar-all =
          pkgs.linkFarm "grammar-all" [
            {
              name = "modules/parser/src/main/resources";
              path = pkgs.callPackage ts-lib.make-grammar-resources {
                package = system:
                  let pkgs = import nixpkgs { inherit system; }; in
                  pkgs.callPackage ts-lib.rename-grammar {
                    pname = "tree-sitter-smithyql";
                    grammar = self.packages.${system}.grammar;
                    rename-dependencies = true;
                  };
              };
            }
          ];
      }
    );
}
