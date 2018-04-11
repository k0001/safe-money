{ mkDerivation, stdenv, ghc
, base, bytestring, constraints, tasty, tasty-hunit, tasty-quickcheck
, binary, aeson, cereal, deepseq, hashable, serialise, store, xmlbf, text
, flags ? {}
}:

let
flags' =
  { aeson = true;
    binary = true;
    cereal = true;
    deepseq = true;
    hashable = true;
    serialise = true;
    store = true;
    xmlbf = true;
  } // flags;
lib = stdenv.lib;
extraDeps =
  lib.optionals (flags'.aeson) [ aeson ] ++
  lib.optionals (flags'.binary) [ binary ] ++
  lib.optionals (flags'.cereal) [ cereal ] ++
  lib.optionals (flags'.deepseq) [ deepseq ] ++
  lib.optionals (flags'.hashable) [ hashable ] ++
  lib.optionals (flags'.serialise) [ serialise ] ++
  lib.optionals (flags'.store) [ store ] ++
  lib.optionals (flags'.xmlbf) [ xmlbf text ];

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
    lib.mapAttrs (k: v: "-f" + lib.optionalString (!v) "-" + k) flags';
}
