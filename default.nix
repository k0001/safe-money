{ mkDerivation, aeson, base, binary, bytestring, cereal
, constraints, deepseq, hashable, stdenv, store, tasty, tasty-hunit
, tasty-quickcheck
}:
mkDerivation {
  pname = "safe-money";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary cereal constraints deepseq hashable store
  ];
  testHaskellDepends = [
    aeson base binary bytestring cereal constraints deepseq hashable
    store tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/safe-money";
  description = "Type-safe and lossless encoding and manipulation of money, world currencies and precious metals";
  license = stdenv.lib.licenses.bsd3;
}
