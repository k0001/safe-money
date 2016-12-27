let
_pkgs0 = import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "d82a1abbb4dc2d9ed914535efcc7ba53037ce525";
  sha256 = "16a2qxyb1ahqd5xl2ih99250vz4msbh103r5bqv0564nqzsmq4q2";
}) {};

in
{ pkgs ? _pkgs0
, compiler ? "ghc801"
}:

let
hsLib = pkgs.haskell.lib;
hs = pkgs.haskell.packages.${compiler}.override {
  overrides = self: super: {
    tasty-ant-xml = hsLib.doJailbreak super.tasty-ant-xml;
  };
};

drv = hs.callPackage (import ./pkg.nix) {};

in
if pkgs.lib.inNixShell then drv.env else drv
