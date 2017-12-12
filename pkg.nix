{ mkDerivation, stdenv, ghc
, base, bytestring, constraints, tasty, tasty-hunit, tasty-quickcheck


# Optional dependencies
, aeson ? null
, binary ? null
, cereal ? null
, deepseq ? null
, hashable ? null
, serialise ? null
, store ? null
, xmlbf ? null
, text ? null
}:

assert !(isNull xmlbf) -> !(isNull text);

let
extraDeps =
  stdenv.lib.optionals (!(isNull aeson)) [ aeson ] ++
  stdenv.lib.optionals (!(isNull binary)) [ binary ] ++
  stdenv.lib.optionals (!(isNull cereal)) [ cereal ] ++
  stdenv.lib.optionals (!(isNull deepseq)) [ deepseq ] ++
  stdenv.lib.optionals (!(isNull hashable)) [ hashable ] ++
  stdenv.lib.optionals (!(isNull serialise)) [ serialise ] ++
  stdenv.lib.optionals (!(isNull store)) [ store ] ++
  stdenv.lib.optionals (!(isNull xmlbf)) [ xmlbf text ];

in mkDerivation rec {
  pname = "safe-money";
  version = "0.4";
  homepage = "https://github.com/k0001/safe-money";
  description = "Type-safe and lossless encoding and manipulation of money, currencies and precious metals";
  license = stdenv.lib.licenses.bsd3;
  src = ./.;
  libraryHaskellDepends = [ base constraints ] ++ extraDeps;
  testHaskellDepends = libraryHaskellDepends ++
    [ bytestring tasty tasty-hunit tasty-quickcheck ];
  configureFlags =
    stdenv.lib.optionals (isNull aeson) [ "-f-aeson" ] ++
    stdenv.lib.optionals (isNull binary) [ "-f-binary" ] ++
    stdenv.lib.optionals (isNull cereal) [ "-f-cereal" ] ++
    stdenv.lib.optionals (isNull deepseq) [ "-f-deepseq" ] ++
    stdenv.lib.optionals (isNull hashable) [ "-f-hashable" ] ++
    stdenv.lib.optionals (isNull serialise) [ "-f-serialise" ] ++
    stdenv.lib.optionals (isNull store) [ "-f-store" ] ++
    stdenv.lib.optionals (isNull xmlbf) [ "-f-xmlbf" ];
}
