{ mkDerivation, base, binary, bytestring, constraints, deepseq
, hashable, QuickCheck, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text, vector-space
}:
mkDerivation {
  pname = "safe-money";
  version = "0.9";
  src = ./.;
  libraryHaskellDepends = [
    base binary constraints deepseq hashable QuickCheck text
    vector-space
  ];
  testHaskellDepends = [
    base binary bytestring constraints deepseq hashable tasty
    tasty-hunit tasty-quickcheck text vector-space
  ];
  homepage = "https://github.com/k0001/safe-money";
  description = "Type-safe and lossless encoding and manipulation of money, fiat currencies, crypto currencies and precious metals";
  license = stdenv.lib.licenses.bsd3;
}
