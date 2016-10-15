{ mkDerivation, base, tasty, tasty-hunit, tasty-quickcheck, stdenv
}:
mkDerivation {
  pname = "money";
  version = "0.1";
  src = ./.;
  isExcecutable = false;
  libraryHaskellDepends = [ base scientific ];
  testDepends = [ base tasty tasty-hunit tasty-quickcheck ];
  homepage = "https://github.com/k0001/haskell-money";
  description = "Type-safe encoding and manipulation of world currencies and precious metals";
  license = stdenv.lib.licenses.bsd3;
}
