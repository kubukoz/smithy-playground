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
        suffix = system: (pkgs.lib.systems.elaborate system).extensions.sharedLibrary;
        grammar = pkgs.stdenv.mkDerivation {
          pname = "tree-sitter-smithyql";
          version = "0.0.0";
          src = ./tree-sitter-smithyql;
          buildInputs = [ pkgs.tree-sitter pkgs.nodejs ];
          FILENAME = "libtree-sitter-smithyql${suffix system}";
          buildPhase = ''
            tree-sitter generate
            cc src/parser.c -o $FILENAME -Isrc -shared
          '';
          installPhase = ''
            cp $FILENAME $out
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
        packages.grammar-all =
          let system-mappings = {
            "darwin-x86_64" = "x86_64-darwin";
            "darwin-aarch64" = "aarch64-darwin";
            "linux-x86_64" = "x86_64-linux";
            "linux-aarch64" = "aarch64-linux";
          }; in
          pkgs.linkFarm "grammar-all" (pkgs.lib.mapAttrsToList
            (jna-system: nix-system:
              let suffix = (pkgs.lib.systems.elaborate nix-system).extensions.sharedLibrary; in
              {
                name = "${jna-system}/${self.packages.${nix-system}.grammar.FILENAME}";
                path = self.packages.${nix-system}.grammar;
              })
            system-mappings);
      }
    );
}
