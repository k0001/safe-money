{ mkDerivation, aeson, base, bytestring, safe-money, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "safe-money-aeson";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [ aeson base safe-money text ];
  testHaskellDepends = [
    aeson base bytestring safe-money tasty tasty-hunit tasty-quickcheck
    text
  ];
  homepage = "https://github.com/k0001/safe-money";
  description = "Instances from the aeson library for the safe-money library";
  license = stdenv.lib.licenses.bsd3;
}
