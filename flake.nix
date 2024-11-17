{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "nixpkgs"; # primary nixpkgs
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskell.compiler.ghc982
            ormolu
            hlint
            haskell-language-server
          ];
        };
      }
    );
}
