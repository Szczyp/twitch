{ pkgs ? import <nixpkgs> {} }:
with pkgs;
haskellPackages.callCabal2nix "twitch" (nix-gitignore.gitignoreSource [] ./.) {}
