# This file exports every derivation introduced by this repository.
{ nixpkgs ? import ./nixpkgs.nix }:
let pkgs = import ./pkgs.nix { inherit nixpkgs; };
in
pkgs.releaseTools.aggregate {
  name = "everything";
  constituents = [
    pkgs._here.ghc8104.safe-money
    pkgs._here.ghc8104.safe-money-aeson
    pkgs._here.ghc8104.safe-money-aeson.doc
    pkgs._here.ghc8104.safe-money-cereal
    pkgs._here.ghc8104.safe-money-cereal.doc
    pkgs._here.ghc8104.safe-money.doc
    pkgs._here.ghc8104.safe-money-serialise
    pkgs._here.ghc8104.safe-money-serialise.doc
    pkgs._here.ghc8104.safe-money-store
    pkgs._here.ghc8104.safe-money-store.doc
    pkgs._here.ghc8104.safe-money-xmlbf
    pkgs._here.ghc8104.safe-money-xmlbf.doc
    pkgs._here.ghc8104._shell
  ];
}

