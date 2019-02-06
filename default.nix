{ compiler ? "ghc844" }:

let
  pkgs = import <nixpkgs> { };

  haskellPackages = pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage ./packages.nix { };
in
  drv
