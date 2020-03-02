{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let
  name = "twitch";

  src = nix-gitignore.gitignoreSource [] ./.;

  hp = haskellPackages.override {
    overrides = self: super: {
      ${name} = self.callCabal2nix name src {};
    };
  };

  drv = haskell.lib.justStaticExecutables hp.${name};

  shell = hp.shellFor {
    withHoogle = true;
    packages = p: [ p.${name} ];
    buildInputs = with hp; [
      cabal-install
      apply-refact
      # hindent
      hlint
      # stylish-haskell
      hasktags
      hoogle
      cabal2nix
      # (import (builtins.fetchTarball "https://github.com/hercules-ci/ghcide-nix/tarball/master") {}).ghcide-ghc865
    ];
  };
in
drv // { inherit shell; }
