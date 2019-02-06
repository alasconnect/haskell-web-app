{ mkDerivation, aeson, base, containers, hspec, mtl, servant
, servant-server, stdenv, streamly, tagged, text, wai, warp
}:
mkDerivation {
  pname = "streamly-experiment";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers mtl servant servant-server streamly tagged
    text wai warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  license = stdenv.lib.licenses.bsd3;
}
