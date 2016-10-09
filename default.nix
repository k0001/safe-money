{ mkDerivation, base, scientific, stdenv }:
mkDerivation {
  pname = "money";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [ base scientific ];
  homepage = "https://github.com/k0001/haskell-money";
  description = "Type-safe encoding and manipulation of world currencies and precious metals";
  license = stdenv.lib.licenses.bsd3;
}
