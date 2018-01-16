{ mkDerivation, base, boxes, classy-prelude, directory, lens
, lens-aeson, MissingH, stdenv, time, wreq, yaml
}:
mkDerivation {
  pname = "twitch";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base boxes classy-prelude directory lens lens-aeson MissingH time
    wreq yaml
  ];
  license = stdenv.lib.licenses.gpl3;
}
