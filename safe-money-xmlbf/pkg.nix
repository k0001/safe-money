{ mkDerivation, base, bytestring, safe-money, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text, xmlbf
}:
mkDerivation {
  pname = "safe-money-xmlbf";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [ base safe-money text xmlbf ];
  testHaskellDepends = [
    base bytestring safe-money tasty tasty-hunit tasty-quickcheck text
    xmlbf
  ];
  homepage = "https://github.com/k0001/safe-money";
  description = "Instances from the xmlbf library for the safe-money library";
  license = stdenv.lib.licenses.bsd3;
}
