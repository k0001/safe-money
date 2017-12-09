{ compiler ? "ghc821" }:
(import ./release.nix {}).${compiler}.safe-money.env
