{ pkgs ? import <nixpkgs> {}, ghc ? "ghc822" }:

let
  drv = pkgs.haskell.packages."${ghc}".callPackage ./. {};

  tools = with pkgs.haskell.packages."${ghc}"; [
    apply-refact
    cabal-install
    cabal2nix
    hasktags
    hindent
    hlint
    hoogle
    stylish-haskell
  ];

in
  pkgs.stdenv.mkDerivation {
    name = "devEnv";
    buildInputs = tools ++ drv.env.buildInputs;
    nativeBuildInputs = drv.env.nativeBuildInputs;
  }


