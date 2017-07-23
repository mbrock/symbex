{ mkDerivation, aeson, base, base16-bytestring, bytestring
, containers, cryptonite, data-dword, lens, mtl, optparse-generic
, rosezipper, s-cargot, stdenv, text, uniplate
, unordered-containers, vector
}:
mkDerivation {
  pname = "symbex";
  version = "0.5.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base16-bytestring bytestring containers cryptonite
    data-dword lens mtl optparse-generic rosezipper s-cargot text
    uniplate unordered-containers vector
  ];
  homepage = "https://github.com/dapphub/symbex";
  description = "Ethereum symbolic execution engine";
  license = stdenv.lib.licenses.agpl3;
}
