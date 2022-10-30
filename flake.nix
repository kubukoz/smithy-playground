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
        grammar =
          let
            rename-grammar = grammar:
              let inherit (grammar) pname; in
              pkgs.stdenv.mkDerivation {
                inherit pname;
                inherit (grammar) version;
                buildCommand =
                  if pkgs.stdenv.isDarwin then ''
                    mkdir -p $out/lib
                    cp ${grammar}/parser $out/lib/lib${pname}.dylib
                    chmod +w $out/lib/lib${pname}.dylib
                    install_name_tool -id lib${pname}.dylib $out/lib/lib${pname}.dylib
                  '' else ''
                    mkdir -p $out/lib
                    cp ${grammar}/parser $out/lib/lib${pname}.so
                  '';
              };

            tree-sitter-smithyql = pkgs.stdenv.mkDerivation {
              pname = "tree-sitter-smithyql";
              version = "0.0.0";
              src = ./tree-sitter-smithyql;
              buildInputs = [ pkgs.tree-sitter pkgs.nodejs ];
              buildPhase = ''
                tree-sitter generate
                cc src/parser.c -o parser -Isrc -shared
              '';
              installPhase = ''
                mkdir -p $out
                cp parser $out/parser
              '';
            };
          in
          rename-grammar tree-sitter-smithyql;

        make-grammar-resources =
          { package
          , system-mappings ? {
              "darwin-x86_64" = "x86_64-darwin";
              "darwin-aarch64" = "aarch64-darwin";
              "linux-x86_64" = "x86_64-linux";
              "linux-aarch64" = "aarch64-linux";
            }
          }: pkgs.linkFarm "${(package system).name}-all" (pkgs.lib.mapAttrsToList
            (jna-system: nix-system:
            let
              suffix = (pkgs.lib.systems.elaborate nix-system).extensions.sharedLibrary;
              pkg = package nix-system;
            in
            {
              name = "${jna-system}";
              path = "${pkg}/lib";
            })
            system-mappings);
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
        packages.grammar-all = make-grammar-resources {
          package = system: self.packages.${system}.grammar;
        };
      }
    );
}
