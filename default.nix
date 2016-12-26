let
_pkgs0 = import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "4aadb9beb345898c7f7b06090b163e7f3debea5a";
  sha256 = "1dka2knhi8pfb83iwph7chldys1df95dc3z2v38cqzy4m06qjir9";
}) {};

in
{ pkgs ? _pkgs0
, compiler ? "ghc801"
}:

let drv = pkgs.haskell.packages.${compiler}.callPackage (import ./pkg.nix) {};
in if pkgs.lib.inNixShell then drv.env else drv
