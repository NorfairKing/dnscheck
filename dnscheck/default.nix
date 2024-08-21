{ mkDerivation, aeson, autodocodec, autodocodec-nix
, autodocodec-yaml, base, dns, genvalidity-bytestring
, genvalidity-sydtest, genvalidity-sydtest-aeson, genvalidity-text
, iproute, lib, path, path-io, QuickCheck, retry, sydtest
, sydtest-discover, text, validity, yaml
}:
mkDerivation {
  pname = "dnscheck";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-nix autodocodec-yaml base dns iproute
    path path-io retry sydtest text validity yaml
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    autodocodec-nix base dns genvalidity-bytestring genvalidity-sydtest
    genvalidity-sydtest-aeson genvalidity-text iproute QuickCheck
    sydtest text
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/dnscheck#readme";
  license = "unknown";
  mainProgram = "dnscheck";
}
