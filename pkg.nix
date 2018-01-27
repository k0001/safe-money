{ mkDerivation, stdenv, ghc
, base, bytestring, constraints, tasty, tasty-hunit, tasty-quickcheck
, binary, aeson, cereal, deepseq, hashable, serialise, store, xmlbf, text

, hasAeson ? true
, hasBinary ? true
, hasCereal ? true
, hasDeepseq ? true
, hasHashable ? true
, hasSerialise ? true
, hasStore ? true
, hasXmlbf ? true
}:

let
extraDeps =
  stdenv.lib.optionals (hasAeson) [ aeson ] ++
  stdenv.lib.optionals (hasBinary) [ binary ] ++
  stdenv.lib.optionals (hasCereal) [ cereal ] ++
  stdenv.lib.optionals (hasDeepseq) [ deepseq ] ++
  stdenv.lib.optionals (hasHashable) [ hashable ] ++
  stdenv.lib.optionals (hasSerialise) [ serialise ] ++
  stdenv.lib.optionals (hasStore) [ store ] ++
  stdenv.lib.optionals (hasXmlbf) [ xmlbf text ];

in mkDerivation rec {
  pname = "safe-money";
  version = "0.4.1";
  homepage = "https://github.com/k0001/safe-money";
  description = "Type-safe and lossless encoding and manipulation of money, currencies and precious metals";
  license = stdenv.lib.licenses.bsd3;
  src = ./.;
  libraryHaskellDepends =
    [ base constraints ] ++ extraDeps;
  testHaskellDepends = libraryHaskellDepends ++
    [ bytestring tasty tasty-hunit tasty-quickcheck ];
  configureFlags =
    stdenv.lib.optionals (!hasAeson) [ "-f-aeson" ] ++
    stdenv.lib.optionals (!hasBinary) [ "-f-binary" ] ++
    stdenv.lib.optionals (!hasCereal) [ "-f-cereal" ] ++
    stdenv.lib.optionals (!hasDeepseq) [ "-f-deepseq" ] ++
    stdenv.lib.optionals (!hasHashable) [ "-f-hashable" ] ++
    stdenv.lib.optionals (!hasSerialise) [ "-f-serialise" ] ++
    stdenv.lib.optionals (!hasStore) [ "-f-store" ] ++
    stdenv.lib.optionals (!hasXmlbf) [ "-f-xmlbf" ];
}
