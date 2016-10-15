let
_pkgs0 = import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "4aadb9beb345898c7f7b06090b163e7f3debea5a";
  sha256 = "0j6ppr4cr3kj8m3xd1kiah3351j1g1v5gzm0vqibj8p5h7490s6j";
});

in
{ pkgs ? _pkgs0
, compiler ? "ghc801"
}:

let
f = import ./default.nix;
haskellPackages = if compiler == "default"
                     then pkgs.haskellPackages
                     else pkgs.haskell.packages.${compiler};
drv = haskellPackages.callPackage f {};
in
if pkgs.lib.inNixShell then drv.env else drv
