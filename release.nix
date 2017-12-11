{ nixpkgs ? import ./nixpkgs-src.nix
, pkgs ? import nixpkgs { system = "x86_64-linux"; }
}:

let x = import ./default.nix { inherit nixpkgs pkgs; };

in rec {
  ghc802 = { inherit (x.ghc802) safe-money safe-money_no-extras; };
  ghc822 = { inherit (x.ghc822) safe-money safe-money_no-extras; };

  everything = pkgs.releaseTools.aggregate {
    name = "everything";
    meta.description = "Every job in release.nix";
    constituents = [
      ghc802.safe-money ghc802.safe-money_no-extras
      ghc822.safe-money ghc822.safe-money_no-extras
    ];
  };
}

