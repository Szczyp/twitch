with (import <nixpkgs> {});
with haskell.packages.ghc7103;
(mkDerivation {
  pname = "twitch";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ aeson base boxes classy-prelude lens lens-aeson wreq];
  buildTools = [ cabal-install ghc-mod stylish-haskell hoogle hasktags hlint ];
  license = stdenv.lib.licenses.gpl3;
}).env
