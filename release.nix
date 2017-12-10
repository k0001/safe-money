{ nixpkgs ? import ./nixpkgs-src.nix
, pkgs ? import nixpkgs { system = "x86_64-linux"; }
}:

let x = import ./default.nix { inherit nixpkgs pkgs; };

in rec {
  ghc802 = { inherit (x.ghc802) safe-money; };
  ghc821 = { inherit (x.ghc821) safe-money; };

  everything = pkgs.releaseTools.aggregate {
    name = "everything";
    meta.description = "Every job in release.nix";
    constituents = [ ghc802.safe-money ghc821.safe-money ];
  };
}

