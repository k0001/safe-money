{ nixpkgs ? import ./nixpkgs-src.nix
, pkgs ? import nixpkgs { system = "x86_64-linux"; }
}:

let
packageSetConfig = self: super: {
  safe-money = super.callPackage ./pkg.nix {};
  # Mostly here just to test whether the thing builds with flags turned off.
  safe-money_no-extras = self.safe-money.override {
    aeson = null;
    binary = null;
    cereal = null;
    deepseq = null;
    hashable = null;
    serialise = null;
    store = null;
  };

  # The default version doesn't compile.
  weigh = self.weigh_0_0_7;
};

ghc802 = pkgs.haskell.packages.ghc802.override { inherit packageSetConfig; };

ghc822 = pkgs.haskell.packages.ghc822.override { inherit packageSetConfig; };

in { inherit ghc802 ghc822; }
