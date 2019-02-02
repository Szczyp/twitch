{ mkDerivation, base, boxes, classy-prelude, directory, lens
, lens-aeson, MissingH, optparse-applicative, stdenv, time, wreq, yaml
}:
mkDerivation {
  pname = "twitch";
  version = "0.1.2.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    boxes
    classy-prelude
    directory
    lens
    lens-aeson
    MissingH
    optparse-applicative
    time
    wreq
    yaml
  ];
  license = stdenv.lib.licenses.gpl3;
}
