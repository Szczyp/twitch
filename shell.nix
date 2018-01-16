{ pkgs ? import <nixpkgs> {}, ghc ? "ghc822" }:

let
  drv = pkgs.haskell.packages."${ghc}".callPackage ./. {};

  tools = with pkgs.haskell.packages."${ghc}"; [
    cabal-install
    hlint
    apply-refact
    hindent
    hasktags
    hoogle
    cabal2nix
  ];

in
  pkgs.stdenv.mkDerivation {
    name = "devEnv";
    buildInputs = tools ++ drv.env.buildInputs;
    nativeBuildInputs = drv.env.nativeBuildInputs;
  }


