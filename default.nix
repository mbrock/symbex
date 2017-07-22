{ mkDerivation, base, base16-bytestring, bytestring, containers
, cryptonite, data-dword, lens, mtl, optparse-generic, rosezipper
, stdenv, text, uniplate, unordered-containers, vector
}:
mkDerivation {
  pname = "symbex";
  version = "0.5.0";
  src = ./.;
  libraryHaskellDepends = [
    base base16-bytestring bytestring containers cryptonite data-dword
    lens mtl optparse-generic rosezipper text uniplate
    unordered-containers vector
  ];
  homepage = "https://github.com/dapphub/symbex";
  description = "Ethereum symbolic execution engine";
  license = stdenv.lib.licenses.agpl3;
}
