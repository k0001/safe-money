{ mkDerivation, base, bytestring, safe-money, serialise, stdenv
, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "safe-money-serialise";
  version = "0.1.1";
  src = ./.;
  libraryHaskellDepends = [ base bytestring safe-money serialise ];
  testHaskellDepends = [
    base bytestring safe-money serialise tasty tasty-hunit
    tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/safe-money";
  description = "Instances from the serialise library for the safe-money library";
  license = stdenv.lib.licenses.bsd3;
}
