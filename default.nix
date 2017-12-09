{ nixpkgs ? import ./nixpkgs-src.nix
, pkgs ? import nixpkgs { system = "x86_64-linux"; }
}:

let
packageSetConfig = self: super: {
  safe-money = super.callPackage ./pkg.nix {};
};

ghc802 = pkgs.haskell.packages.ghc802.override { inherit packageSetConfig; };

ghc821 = pkgs.haskell.packages.ghc821.override { inherit packageSetConfig; };

ghcjsHEAD = pkgs.haskell.packages.ghcjsHEAD.override { inherit packageSetConfig; };

in { inherit ghc802 ghc821 ghcjsHEAD; }
