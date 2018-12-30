{ mkDerivation, base, broch, containers, data-default-generics
, filepath, hakyll, pandoc, reroute, stdenv, text, time, wai-extra
, warp
}:
mkDerivation {
  pname = "broch-io";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base broch containers data-default-generics filepath hakyll pandoc
    reroute text time wai-extra warp
  ];
  license = stdenv.lib.licenses.bsd3;
  hydraPlatforms = stdenv.lib.platforms.none;
}
