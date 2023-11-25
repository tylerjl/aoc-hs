{
  description = "Advent of Code solutions in Haskell";

  nixConfig = {
  # This sets the flake to use the IOG nix cache.
  # Nix should ask for permission before using it,
  # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
  };

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          adventofcode =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc928";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = {};
                hindent = {};
                hlint = {};
                haskell-language-server = {};
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; ([
                cabal-install
                cachix
                lz4
              ] ++ (with haskellPackages; [
                haskell-debug-adapter
                hspec-discover
              ]));
              modules = [
                { packages.markup-parse.doHaddock = false; }
              ];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.adventofcode.flake {};
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."adventofcode:exe:adventofcode";
    });
}
