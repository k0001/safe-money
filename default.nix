{ stdenv, mkDerivation
, base, tasty, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "money";
  version = "0.1";
  src = ./.;
  isExecutable = false;
  libraryHaskellDepends = [ base text ];
  testDepends = [ base tasty tasty-hunit tasty-quickcheck text ];
  homepage = "https://github.com/k0001/haskell-money";
  description = "Type-safe encoding and manipulation of world currencies and precious metals";
  license = stdenv.lib.licenses.bsd3;
}
