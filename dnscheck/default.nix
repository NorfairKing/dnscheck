{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base
, bytestring, dns, genvalidity-bytestring, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-text, iproute, lib, path
, path-io, QuickCheck, retry, sydtest, sydtest-discover, text
, validity, validity-bytestring, yaml
}:
mkDerivation {
  pname = "dnscheck";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-yaml base bytestring dns iproute path
    path-io retry sydtest text validity validity-bytestring yaml
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base dns genvalidity-bytestring genvalidity-sydtest
    genvalidity-sydtest-aeson genvalidity-text iproute QuickCheck
    sydtest text
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/dnscheck#readme";
  license = "unknown";
  mainProgram = "dnscheck";
}
