{ mkDerivation, base, bytestring, safe-money, stdenv, store, tasty
, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "safe-money-store";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [ base bytestring safe-money store ];
  testHaskellDepends = [
    base bytestring safe-money store tasty tasty-hunit tasty-quickcheck
    text
  ];
  homepage = "https://github.com/k0001/safe-money";
  description = "Instances from the store library for the safe-money library";
  license = stdenv.lib.licenses.bsd3;
}
