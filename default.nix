{ nixpkgs ? import ./nixpkgs-src.nix
, pkgs ? import nixpkgs { system = "x86_64-linux"; }
}:

let

src_xmlbf = "${pkgs.fetchgit {
  url = "https://gitlab.com/k0001/xmlbf.git";
  rev = "xmlbf-0.2";
  sha256 = "00llajlr7w102xlwnp09yfmxf7mjx5x2g9f219q28wy423r5vsar";
}}/xmlbf";

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
  # Not in upstream nixpkgs yet.
  xmlbf = super.callPackage "${src_xmlbf}/pkg.nix" {};
};

ghc802 = pkgs.haskell.packages.ghc802.override { inherit packageSetConfig; };

ghc822 = pkgs.haskell.packages.ghc822.override { inherit packageSetConfig; };

in { inherit ghc802 ghc822; }
