{ pkgs }:

let
hsLib = pkgs.haskell.lib;
src-xmlbf = builtins.fetchGit {
  url = "https://gitlab.com/k0001/xmlbf.git";
  rev = "cf66dd4ef2f691b575c03c6f19aee1109cf55be6";
};

in
# This expression can be used as a Haskell package set `packageSetConfig`:
pkgs.lib.composeExtensions
  (import "${src-xmlbf}/hs-overlay.nix")
  (self: super: {
     # The default version doesn't compile.
     vector-space = self.vector-space_0_13;
     # The default version doesn't compile.
     weigh = self.weigh_0_0_7;
     # Too constrained test dependencies
     serialise = hsLib.doJailbreak super.serialise;

     safe-money = super.callPackage ./pkg.nix {};
     # Mostly here just to test whether the thing builds with flags turned off.
     safe-money_no-extras = self.safe-money.override {
       flags = {
         aeson = false;
         cereal = false;
         hashable = false;
         serialise = false;
         store = false;
         vector-space = false;
         xmlbf = false;
       };
     };
  })
