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

        # begin library code
        rename-grammar = { system, lib, stdenv, libcxxabi, libcxx, grammar, library-name }:
          stdenv.mkDerivation {
            inherit (grammar) pname version;
            src = grammar;
            buildPhase =
              if stdenv.isDarwin then ''
                install_name_tool -id lib${library-name}.dylib parser
                install_name_tool -change ${libcxxabi}/lib/libc++abi.1.dylib @loader_path/libc++abi.1.dylib parser
                install_name_tool -change ${libcxx}/lib/libc++.1.0.dylib @loader_path/libc++.1.0.dylib parser
              '' else "true";
            installPhase =
              let suffix = (lib.systems.elaborate system).extensions.sharedLibrary; in
              ''
                mkdir -p $out/lib
                cp parser $out/lib/lib${library-name}${suffix}
              '';
          };

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
        # end library code
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
        packages.grammar-generated = pkgs.stdenv.mkDerivation {
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
        };
        packages.grammar =
          let base = self.packages.${system}.grammar-generated; in
          pkgs.callPackage "${nixpkgs}/pkgs/development/tools/parsing/tree-sitter/grammar.nix" { } {
            language = "smithyql";
            inherit (base) version;
            source = base;
          };
        packages.grammar-all = make-grammar-resources {
          package = system:
            let pkgs = import nixpkgs { inherit system; }; in
            pkgs.callPackage rename-grammar {
              grammar = self.packages.${system}.grammar;
              library-name = "tree-sitter-smithyql";
            };
        };
      }
    );
}
