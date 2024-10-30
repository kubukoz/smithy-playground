{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
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
            pkgs.tree-sitter
            # temporary, while we don't download coursier ourselves
            pkgs.coursier
          ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.xvfb-run ];
        };
        packages.tree-sitter-smithyql =
          pkgs.stdenv.mkDerivation {
            name = "tree-sitter-smithyql";
            src = ./tree-sitter-smithyql;
            buildInputs = [pkgs.tree-sitter pkgs.nodejs];
            buildPhase = ''
              ls -ll
              tree-sitter generate
              make
              ls -ll
            '';
            installPhase = if system == "x86_64-darwin" || system == "aarch64-darwin" then ''
              cp libtree-sitter-smithyql.dylib $out
            '' else ''
              cp libtree-sitter-smithyql.so $out
            '';
          };
      }
    );
}
