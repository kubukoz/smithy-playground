{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.yarn
            pkgs.nodejs
            pkgs.sbt
            pkgs.jless
            pkgs.gnupg
            (pkgs.tree-sitter.override { webUISupport = true; })
            # temporary, while we don't download coursier ourselves
            pkgs.coursier
          ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.xvfb-run ];
        };
        packages.tree-sitter-smithyql = pkgs.stdenv.mkDerivation {
          name = "tree-sitter-smithyql";
          src = ./tree-sitter-smithyql;
          buildInputs = [ pkgs.tree-sitter pkgs.nodejs ];
          buildPhase = ''
            tree-sitter generate
            cc src/parser.c -shared -o $out
          '';
          dontInstall = true;
        };
        packages.tree-sitter-smithyql-all = pkgs.stdenv.mkDerivation {
          name = "tree-sitter-smithyql-all";
          src = ./tree-sitter-smithyql;
          dontBuild = true;
          installPhase = ''
            mkdir $out
            cd $out
            mkdir darwin-aarch64 && cp ${self.packages.aarch64-darwin.tree-sitter-smithyql} darwin-aarch64/libtree-sitter-smithyql.dylib
            mkdir darwin-x86-64 && cp ${self.packages.x86_64-darwin.tree-sitter-smithyql} darwin-x86-64/libtree-sitter-smithyql.dylib
            mkdir linux-aarch64 && cp ${self.packages.aarch64-linux.tree-sitter-smithyql} linux-aarch64/libtree-sitter-smithyql.so
            mkdir linux-x86-64 && cp ${self.packages.x86_64-linux.tree-sitter-smithyql} linux-x86-64/libtree-sitter-smithyql.so
          '';
        };
      });
}
