{ mkDerivation, base, bytestring, cereal, safe-money, stdenv, tasty
, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "safe-money-cereal";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [ base cereal safe-money ];
  testHaskellDepends = [
    base bytestring cereal safe-money tasty tasty-hunit
    tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/safe-money";
  description = "Instances from the cereal library for the safe-money library";
  license = stdenv.lib.licenses.bsd3;
}
