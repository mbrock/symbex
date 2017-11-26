{ mkDerivation, aeson, base, bytestring, containers, mtl, s-cargot
, stdenv, text, uniplate, vector
}:
mkDerivation {
  pname = "symbex";
  version = "0.5.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers mtl s-cargot text uniplate vector
  ];
  homepage = "https://github.com/dapphub/symbex";
  description = "Ethereum symbolic execution engine";
  license = stdenv.lib.licenses.agpl3;
}
