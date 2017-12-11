{ compiler ? "ghc822" }:
(import ./release.nix {}).${compiler}.safe-money.env
